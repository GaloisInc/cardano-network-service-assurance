{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.Tracer.CNSA.CnsaAnalyses
  ( mkCnsaSinkAnalyses
  )
where

-- base:
import           Control.Monad
import           Data.Functor.Contravariant
import           Data.List (isPrefixOf)
import qualified Data.Map as Map
import           Data.Set (Set)
import           Data.Text (Text)
import           Data.Time (nominalDiffTimeToSeconds,diffUTCTime,NominalDiffTime)
import           GHC.Generics

-- package contra-tracer: (not to be confused with Cardano.Tracer....)
import qualified "contra-tracer" Control.Tracer as OrigCT

-- package aeson:
import           Data.Aeson

-- cardano packages:
import           Cardano.Logging.Trace
import           Cardano.Logging.Tracer.DataPoint
import qualified Cardano.Logging.Types as Log
import           Cardano.Slotting.Slot
import           Cardano.Tracer.MetaTrace -- hiding (traceWith)
import           Trace.Forward.Utils.DataPoint

-- package locli: (or slice thereof)
import qualified Cardano.Unlog.LogObject as LO
import           Cardano.Analysis.API.Ground (Hash)

-- package prometheus:
import qualified System.Metrics.Prometheus.Concurrent.Registry as PR
import qualified System.Metrics.Prometheus.Metric.Counter      as PC
import qualified System.Metrics.Prometheus.Metric.Gauge        as PG
import           System.Metrics.Prometheus.Http.Scrape         as PS
import qualified System.Metrics.Prometheus.Metric.Histogram    as PH

-- local to this pkg:
import           Cardano.Tracer.CNSA.BlockState
import           Cardano.Tracer.CNSA.BlockState.DB (BlockDBHandle, initializeBlockDB, writeBlockData)
import           Cardano.Tracer.CNSA.ParseLogs
import           Cardano.Utils.Log
import           Cardano.Utils.SlotTimes


------------------------------------------------------------------------------
-- Types

data AnalysisArgs = AnalysisArgs
  { aaRegistry :: PR.Registry             -- ^ prometheus registry
  , aaTraceDP  :: Trace IO DataPoint      -- ^ toplevel datapoint trace
  , aaDebugTr  :: OrigCT.Tracer IO String -- ^ cnsa debugging tracer
  -- TODO: see Improvement 3. below, replace stdout with this:
  -- , aaLogTr    :: OrigCT.Tracer IO String -- ^ the log for this analysis
  -- ?
  }

--------------------------------------------------------------------------------
-- Namespaces
--
-- Some of this is duplicative of code in `Cardano.Tracer.Handlers.ReForwarder`,
-- but not entirely, since that code operates on lists. We should porobably
-- upstream this version of the abstraction.

-- | The log name space, corresponds to Log.toNamespace field.
type Namespace = [Text]  --

-- | A filter for namespaces - query it with `nsFilterAllows`.
--
-- XXX: want Semigroup, Monoid, IsList, perhaps `insert`
data NamespaceFilter
  = NoFilter
  | Filter (Set Namespace)

-- | Does the `NamespaceFilter` allow the `Namespace`?
nsFilterAllows :: NamespaceFilter -> Namespace -> Bool
nsFilterAllows nsf containee =
  case nsf of
    NoFilter -> True
    Filter containers -> any (`nsContains` containee) containers

-- | Does `container` contain `containee`?
--
-- >>> ["Foo", "Bar"] `nsContains` ["Foo", "Bar", "Baz"]
-- True
--
-- >>> ["Foo", "Bar"] `nsContains` ["Foo", "Bar"]
-- True
--
-- >>> ["Foo", "Bar"] `nsContains` ["Foo"]
-- False
nsContains :: Namespace -> Namespace -> Bool
nsContains container containee = container `isPrefixOf` containee

--------------------------------------------------------------------------------

-- | Analysis - capture a 'generic' analysis that receives traceobjects
--              and updates datapoints, logs ..., and serves prometheus
--              data.
--
-- FIXME[F2]: Improvements
--  1. use/implement aTraceNames
--      - use code from updated cardano-tracer to filter and combine.
--     SamC: done, sorta, see caveat in Namespaces section above
--  2. here or _: build in code that does "overflowing" datapoints
--  3. stop writing to stdout, but ...
--    - each analysis has own file
--    - each analysis has own prefix in one logfile/logsystem?
--    - debugTracing (aaDebugTr) vs stdout vs _: get all in order
--  4. datapoint 'abstractions/improvements
--     - rather than adhoc Log.MetaTrace, can you ...
--       - enumerate datapoint types & names in Analysis?
--       - ...?
--  5. other conveniences
--    - turn [scalar] datapoints [easily/automatically] into prometheus data
--    - ?
--  6. add database hooks, e.g., a new field below:
--      aDataBaseHook       :: DBObject -> IO ()

data Analysis = forall state. Analysis
  { aName               :: String
  , aTraceNamespaces    :: NamespaceFilter
  , aInitialize         :: AnalysisArgs -> IO (Possibly state)
  , aProcessTraceObject :: AnalysisArgs
                        -> state
                        -> Log.TraceObject -> LO.LOBody -> IO ()
  }


------------------------------------------------------------------------------
-- All CNSA Analyses:

analyses :: [Analysis]
analyses = [ countTraceLogsAnalysis
           , blockStatusAnalysis
           ]

------------------------------------------------------------------------------
-- Analysis abstractions:

-- | initialize one analysis
initAnalysis :: AnalysisArgs
             -> Analysis
             -> IO (Log.TraceObject -> LO.LOBody -> IO ())
initAnalysis args Analysis{aInitialize,aName,aTraceNamespaces,aProcessTraceObject} =
  do
    ms <- aInitialize args
    putStrLn $ "Initializing analysis '" ++ aName ++ "'"
    case ms of
      Left ss -> error $ unlines ("Failure:" : ss)
      Right s -> return $
        \traceObj logBody ->
          when (aTraceNamespaces `nsFilterAllows` Log.toNamespace traceObj) $
            aProcessTraceObject args s traceObj logBody

------------------------------------------------------------------------------
-- our toplevel

-- | initialize all analyses and return generic handlers:
mkCnsaSinkAnalyses :: Trace IO DataPoint
                   -> OrigCT.Tracer IO String
                   -> IO (Log.TraceObject -> IO (), IO ())
mkCnsaSinkAnalyses traceDP debugTr =
  do
  registry <- PR.new
  let args = AnalysisArgs{aaRegistry= registry
                         ,aaTraceDP = traceDP
                         ,aaDebugTr = debugTr
                         }

  -- Consolidate all analyses:
  as' <- mapM (initAnalysis args) analyses
  let sendTraceLogObjectToAnalyses o b = mapM_ (\p-> p o b) as'
    -- FIXME[E2]: use the aTraceNames Set to make more efficient.

  -- Return code that delegates/distributes to all analyses:
  return
    ( -- handle TraceObject:
      \trObj->
        do
        OrigCT.traceWith debugTr ("recvd traceLogObj: " ++ show trObj)
        case getLogBody trObj of
          Left s -> warnMsg
                      [ "unparseable TraceObject:", s]
          Right lb ->
              do
              OrigCT.traceWith debugTr ("parsed log body: " ++ show lb)
              case lb of
                LB_LOBody lb' -> sendTraceLogObjectToAnalyses trObj lb'
                LB_Etc    _   -> return ()

    , PS.serveMetrics 8080 ["metrics"] (PR.sample registry)
        -- http://localhost:8080/metrics
        -- FIXME[F1]: configure port
    )


------------------------------------------------------------------------------
-- a trivial "proof of life" analysis:
--

countTraceLogsAnalysis :: Analysis
countTraceLogsAnalysis =
  Analysis{ aName = "count trace logs"
          , aTraceNamespaces = NoFilter
          , aInitialize = initialize
          , aProcessTraceObject = pto
          }
  where
    initialize args = do
      metric_traceCtr <-
        PR.registerCounter "count_of_tracelogs" mempty (aaRegistry args)
      return (Right metric_traceCtr)

    pto _ metric_traceCtr _trobj _parsedlog =
      PC.inc metric_traceCtr


------------------------------------------------------------------------------
-- The block status analysis:
--

blockStatusAnalysis :: Analysis
blockStatusAnalysis =
  Analysis { aName=               "Block Status"
           , aTraceNamespaces=    NoFilter
           , aInitialize=         initializeBlockStatusAnalysis
           , aProcessTraceObject= processBlockStatusAnalysis
           }

-- If we get any of the 'other' messages before we see the downloaded header,
-- we print warnings and ignore the log message.

-- type Delay = Int      -- MSecs -- TODO

data BlockAnalysisState = BlockAnalysisState
  { asBlockStateHdl   :: BlockStateHdl,
    asBlockDBHdl      :: Maybe BlockDBHandle,
    asBlockStateTrace :: Trace IO BlockState,
    asTopSlotGauge    :: PG.Gauge,
    asPenultSlotGauge :: PG.Gauge,
    asPropDelaysHist  :: PH.Histogram
  }

initializeBlockStatusAnalysis
  :: AnalysisArgs -> IO (Possibly BlockAnalysisState)
initializeBlockStatusAnalysis (AnalysisArgs registry traceDP _) =
  do
    blockStateHdl   <- newBlockStateHdl
    blockDBHdl      <- initializeBlockDB "blocks" >>= \case
      Left err -> putStrLn err >> pure Nothing
      Right hdl -> pure (Just hdl)
    blockStateTrace <- contramap BlockState' <$> mkDataPointTracer traceDP
    topSlotGauge    <- PR.registerGauge     "slot_top"         mempty registry
    penultSlotGauge <- PR.registerGauge     "slot_penultimate" mempty registry
    propDelaysHist  <- PR.registerHistogram "propDelays"
                         mempty buckets registry
    pure $ Right $
      BlockAnalysisState
        { asBlockStateHdl   = blockStateHdl,
          asBlockDBHdl      = blockDBHdl,
          asBlockStateTrace = blockStateTrace,
          asTopSlotGauge    = topSlotGauge,
          asPenultSlotGauge = penultSlotGauge,
          asPropDelaysHist  = propDelaysHist
        }
  where
    buckets = [0.1,0.2..2.0]

processBlockStatusAnalysis
  :: AnalysisArgs
  -> BlockAnalysisState
  -> Log.TraceObject -> LO.LOBody -> IO ()
processBlockStatusAnalysis aArgs state trObj logObj =
  do
  -- update the BlockState:
  updateBlockStateFromTraceObject

  -- debugTracing:
  sorted <- sortBySlot <$> readBlockStateHdl blockStateHdl
  debugTraceBlockData "blockState[pre]" sorted

  -- update 'blockStateHdl', removing overflow:
  overflowList <- pruneOverflow blockStateHdl

  -- update blockState Datapoint:
  readBlockStateHdl blockStateHdl >>= traceWith (asBlockStateTrace state)

  -- Process 'overflowList':
  if null overflowList then
    OrigCT.traceWith debugTr "Overflow: none"
  else
    let toDB = writeBlockData
        toStdout (hash, blockData) = OrigCT.traceWith debugTr ("Overflow: " ++ show (hash, blockData))
        write = maybe toStdout toDB blockDBHdl
    in  mapM_ write overflowList

  -- Update Metrics:
  sorted' <- sortBySlot <$> readBlockStateHdl blockStateHdl
  debugTraceBlockData "blockState[post]" sorted'
  case map snd sorted' of
    b0:b1:_ ->
        do
        let
          SlotNo slot0 = bp_slot $ bd_props b0
          SlotNo slot1 = bp_slot $ bd_props b1

        -- update slot metrics:
        PG.set (fromIntegral slot0) (asTopSlotGauge state)
        PG.set (fromIntegral slot1) (asPenultSlotGauge state)
          -- NOTE: No integers? everything a float?!
          --       See node's prometheus output: has integers

        -- update propagation metrics for b1/slot1 (penultimate):
        let
          delays :: [NominalDiffTime]
          delays = map
                     (\t-> diffUTCTime t (slotStart (SlotNo slot1)))
                     (  concatMap (Map.elems . bt_downloadedHeader)
                      $ Map.elems
                      $ bd_timing b1
                      )
        OrigCT.traceWith debugTr $ unwords ["slot_top:", show slot0]
        OrigCT.traceWith debugTr $ unwords ["slot_pen:", show slot1]
        OrigCT.traceWith debugTr $ unwords ["delays:"  , show delays]
        when (any (< 0) delays) $
          errorMsg ["Negative Delay"]
        mapM_ ((\v-> PH.observe v (asPropDelaysHist state)) . cvtTime) delays

    -- unless we have at least two blocks recorded, do nothing:
    _ ->
        return ()

  where
    debugTr = aaDebugTr aArgs

    blockStateHdl = asBlockStateHdl state
    blockDBHdl = asBlockDBHdl state

    updateBlockStateFromTraceObject =
      case logObj of
        LO.LOChainSyncClientSeenHeader slotno blockno hash ->
            withPeer trObj $ \peer->
              modifyBlockStateIOMaybe blockStateHdl $
                addSeenHeader shost slotno blockno hash peer time

        LO.LOBlockFetchClientRequested hash len ->
            withPeer trObj $ \peer->
              updateBSByKey hash
                (addFetchRequest shost len peer time)

        LO.LOBlockFetchClientCompletedFetch hash ->
            withPeer trObj $ \peer->
              updateBSByKey hash
                (addFetchCompleted shost peer time)

        LO.LOBlockAddedToCurrentChain hash msize len ->
            updateBSByKey hash
              (addAddedToCurrentChain shost msize len time)

        _ -> return ()

      where

        updateBSByKey :: Hash -> (BlockData -> Possibly BlockData) -> IO ()
        updateBSByKey = updateBlockStateByKey blockStateHdl

        time  = Log.toTimestamp trObj
        shost = Log.toHostname  trObj -- the sampler host

    debugTraceBlockData nm es =
      do
        OrigCT.traceWith debugTr (nm ++ " block data:")
        mapM_ (OrigCT.traceWith debugTr . show) es
        OrigCT.traceWith debugTr ""
        -- FIXME[F3]: make fancier

    cvtTime = fromRational . toRational . nominalDiffTimeToSeconds

withPeer :: Log.TraceObject -> (Peer -> IO ()) -> IO ()
withPeer trObj f =
  case getPeerFromTraceObject trObj of
    Left s  -> warnMsg ["expected peer, ignoring traceObj: " ++ s]
    Right p -> f p


---- Boilerplate for BlockState' Datapoint -------------------------

newtype BlockState' = BlockState' BlockState
                      deriving (Eq,Ord,Show,Generic)

deriving instance ToJSON BlockState'

instance Log.MetaTrace BlockState'
  where
  namespaceFor _  = Log.Namespace [] ["CNSA","BlockState"]
  severityFor _ _ = Just Info
  documentFor _   = Just "Map, by hash, containing block and propagation info"
  allNamespaces   = [Log.namespaceFor (undefined :: BlockState')]
