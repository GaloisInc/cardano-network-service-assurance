{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Cardano.Tracer.CNSA.CnsaAnalyses
 ( mkCnsaSinkAnalyses
 )
where

import           Control.Monad
import           Data.Aeson
import           Data.Functor.Contravariant
import           Data.IORef
import           Data.List (sortOn,splitAt,foldl',partition)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict(Map)
import qualified Data.Text as Text
import           Data.Time
import           Data.Time.Format (parseTimeOrError)
import           Data.Time.Clock (UTCTime)
import           GHC.Generics
import           Network.HostName (HostName)

import qualified Cardano.Logging.Types as Log
import           Cardano.Logging.Trace
import           Cardano.Logging.Tracer.DataPoint
import           Cardano.Slotting.Block
import           Cardano.Slotting.Slot
import           Cardano.Slotting.Time
import           Cardano.Tracer.MetaTrace
import           Trace.Forward.Utils.DataPoint

-- package locli:
import           Cardano.Analysis.API.Ground (Hash,Host)
import qualified Cardano.Unlog.LogObject as LO

-- package prometheus:
import qualified System.Metrics.Prometheus.Concurrent.Registry as PR
import qualified System.Metrics.Prometheus.Metric.Counter      as PC
import qualified System.Metrics.Prometheus.Metric.Gauge        as PG
import           System.Metrics.Prometheus.Http.Scrape         as PS
import qualified System.Metrics.Prometheus.Metric.Histogram    as PH

-- local to this pkg:
import           Cardano.Tracer.CNSA.ParseLogs


------------------------------------------------------------------------------
-- Create *all* CNSA Sink Analyses
--

mkCnsaSinkAnalyses :: Trace IO DataPoint
                   -> IO (Log.TraceObject -> IO (), IO ())
mkCnsaSinkAnalyses traceDP =
  do
  registry <- PR.new

  traceCompletedBlockFetchTimes <- mkCompletedBlockFetchTrace traceDP

  -- trivial 'proof of life' metric:
  metric_traceCtr <- PR.registerCounter "count_of_tracelogs" mempty registry

  sendTraceLogObj <- mkBlockStatusAnalysis traceDP registry
  return (
           -- handle TraceObject:
           \trObj->
             do
             traceWith traceCompletedBlockFetchTimes trObj
             PC.inc metric_traceCtr
             print trObj          -- Debugging
             case getLogBody trObj of
               Left s -> putStrLn $ unlines
                           [ "Warn: unparsed TraceObject:"
                           , "  " ++ s
                           ]
               Right lb ->
                   do
                   print lb -- Debugging
                   case lb of
                     LB_LOBody l -> sendTraceLogObj trObj l
                     LB_Etc    l -> return ()
             putStrLn ""

         , PS.serveMetrics 8080 ["metrics"] (PR.sample registry)
             -- http://localhost:8080/metrics
         )


------------------------------------------------------------------------------
-- Code for Analysis 2: mkBlockStatusAnalysis
--

type BlockState = Map Hash BlockData

-- FIXME: if we get any of the 'other' messages before we see
-- downloaded header?

type Delay = Int      -- MSecs -- TODO

data BlockData =
  BlockData { bl_blockNo             :: BlockNo
            , bl_slot                :: SlotNo
            , bl_downloadedHeader    :: [(Peer,UTCTime)]
            , bl_sendFetchRequest    :: [(Peer,UTCTime)]
            , bl_completedBlockFetch :: [(Peer,UTCTime)]
              -- KK: CompletedBlockFetch*, this trace is in the wrong
              -- place. We need a trace for when the block has been
              -- downloaded, see #4226.
            , bl_addedToCurrentChain :: Maybe UTCTime
            , bl_size                :: Maybe Int
            }
  deriving (Eq,Ord,Show)

mkBlockStatusAnalysis traceDP registry =
  do
  rawBlockData <- newIORef Map.empty
  topSlotGauge    <- PR.registerGauge     "slot_top"         mempty registry
  penultSlotGauge <- PR.registerGauge     "slot_penultimate" mempty registry
  propDelaysHist  <- PR.registerHistogram "propDelays"       mempty
                       [0.1,0.2..2.0]
                       registry

  let doLogEvent :: Log.TraceObject -> LO.LOBody -> IO ()
      doLogEvent trObj logObj =
        do
        putStrLn "recvd traceLogObj"
        let time = Log.toTimestamp trObj
            withPeer f =
                case getPeerFromTraceObject trObj of
                  Left m  -> putStrLn "warning: expected peer, ignoring trace"
                  Right p -> f p
            host = Log.toHostname trObj
            updBD = modifyIORef' rawBlockData

        -- Update 'rawBlockData :: IORef BlockState' :

        case logObj of 
          LO.LOChainSyncClientSeenHeader slotno blockno hash ->
              withPeer $ \peer->
                updBD (addSeenHeader slotno blockno hash peer time)

          LO.LOBlockFetchClientRequested hash len ->
              withPeer $ \peer->
                updBD (addFetchRequest hash len peer time)
     
          LO.LOBlockFetchClientCompletedFetch hash ->
              withPeer $ \peer->
                updBD (addFetchCompleted hash peer time)

          LO.LOBlockAddedToCurrentChain hash msize len ->
              updBD (addAddedToCurrent hash msize len host time)

        putStrLn "rawBlockData [0]:"
        () <- do
              raw0 <- readIORef rawBlockData
              mapM_ print $ reverse $ sortOn (bl_slot . snd) $ Map.toList raw0

        -- Post-processing:
        overflowList <- atomicModifyIORef'
                          rawBlockData
                          (splitMapOn 3 bl_slot)  -- FIXME: ??
        putStrLn "overflow:"
        mapM_ print overflowList

        raw <- readIORef rawBlockData
        let raw' = reverse $ sortOn (bl_slot . snd)  $ Map.toList raw
            blocksBySlot = map snd raw'
            
        -- Debugging:
        putStrLn "rawBlockData:"
        mapM_ print raw'
        putStrLn ""

        -- Transform/Metrics:
        when (length blocksBySlot > 2) $
          do
          -- slot metrics:
          let
            SlotNo topSlot         = bl_slot (blocksBySlot !! 0)
            SlotNo penultimateSlot = bl_slot (blocksBySlot !! 1)
          PG.set (fromIntegral topSlot        ) topSlotGauge
          PG.set (fromIntegral penultimateSlot) penultSlotGauge
            -- UGH: no integers? everything a float?!
            -- see node's prometheus output: has integers

          -- propagation metrics for penultimateSlot:
          let
            b = blocksBySlot !! 1
            cvtTime = fromRational . toRational . nominalDiffTimeToSeconds
            delays  = map 
                       (\(p,t)->(p, diffUTCTime t (slotStart (bl_slot b))))
                       (bl_downloadedHeader b)
          putStrLn $ unwords ["slot_top:", show topSlot]
          putStrLn $ unwords ["slot_pen:", show penultimateSlot, "; delays:"]
          print delays
          when (any (\(_,d)-> d < 0) delays) $
            putStrLn "ERROR: NEGATIVE DELAY"
          mapM_ (\v-> PH.observe v propDelaysHist)
                (map (cvtTime . snd) delays)

  return doLogEvent

splitMapOn :: (Ord k, Ord b)
           => Int -> (a -> b) -> Map k a -> (Map k a, [(k,a)])
splitMapOn n f m0 = (Map.fromList keepers, overflow)
  where
  (keepers,overflow) = splitAt n $ reverse $ sortOn (f . snd) $ Map.toList m0

test1 =
  do
  let input0 = Map.fromList
             $ [(5-s, defaultBlockData 0 (SlotNo s)) | s <- [0..4]]
  rawBlockData <- newIORef input0
  putStrLn "rawBlockData [0]:"
  () <- do
        raw0 <- readIORef rawBlockData
        mapM_ print $ reverse $ sortOn (bl_slot . snd) $ Map.toList raw0
  overflowList <- atomicModifyIORef'
                    rawBlockData
                    (splitMapOn 4 bl_slot)
  putStrLn "overflow:"
  mapM_ print overflowList

  raw <- readIORef rawBlockData
  let raw' = reverse $ sortOn (bl_slot . snd)  $ Map.toList raw

  -- Debugging:
  putStrLn "rawBlockData:"
  mapM_ print raw'
  putStrLn ""
  
test0 = myPr $ splitMapOn 2 bl_slot input0
input0 = Map.fromList $ [(s, defaultBlockData 0 s) | s <- [0..4]]

test2  = myPr $ splitMapOn 2 bl_slot input2
input2 = Map.fromList $
          [(5-s, defaultBlockData 0 (SlotNo s)) | s <- [0..4]]

myPr (m,xs) = do
              putStrLn "keep:"
              mapM_ print $ Map.toList m
              putStrLn "extra:"
              mapM_ print $ xs
              
defaultBlockData b s =
  BlockData{ bl_blockNo            = b
           , bl_slot               = s
           , bl_downloadedHeader   = []
           , bl_sendFetchRequest   = []
           , bl_completedBlockFetch= []
           , bl_addedToCurrentChain= Nothing
           , bl_size               = Nothing
           }
  
addSeenHeader :: SlotNo
                 -> BlockNo
                 -> Hash
                 -> Peer
                 -> UTCTime
                 -> Map Hash BlockData
                 -> Map Hash BlockData
addSeenHeader slot block hash peer time =
  Map.insertWith
    (\_ o->o{bl_downloadedHeader= bl_downloadedHeader o ++ [(peer,time)]})
    hash
    (defaultBlockData block slot){bl_downloadedHeader=[(peer,time)]}

addFetchRequest _hash _len _peer _time = id

addFetchCompleted _hash _peer _time = id

addAddedToCurrent _hash _msize _len _host _time = id


------------------------------------------------------------------------------
-- 'specifications' for datapoints/etc: ...
--
{-
-- raw, updated on each LogEvent:
rawBlockState :: BlockState

-- derived:
blockPropDelays1 :: (BlockNo, Map Addr Delay)
blockPropDelays  :: Map Hash (Map Addr Delay)
blockHeights     :: Map Addr BlockNo
  -- for each peer, or sampler, the highest blockNo from that peer

(rawBlockState,blockPropDelays,blockPropDelays1,blockHeights) = stub
-}

------------------------------------------------------------------------------
-- Code for [Toy] Analysis 1: mkCompletedBlockFetchTrace
--

mkCompletedBlockFetchTrace ::
  Trace IO DataPoint -> IO (Trace IO Log.TraceObject)
mkCompletedBlockFetchTrace traceDP =
  do
  bfccbfDP :: Trace IO CompletedBlockFetchTimes
    <- mkDataPointTracer traceDP

  let hostTimesTr :: Trace IO HostTimes
      hostTimesTr = contramap CompletedBlockFetchTimes bfccbfDP

  mkLastHostTimeOf
    ["BlockFetch","Client","CompletedBlockFetch"]
    hostTimesTr

mkLastHostTimeOf :: [Text.Text]
                 -> Trace IO HostTimes
                 -> IO (Trace IO Log.TraceObject)
mkLastHostTimeOf ns tr =
  do
  traceWith tr Map.empty  -- Cause this datapoint to immediately have a value.
  foldTraceM compute Map.empty (contramap Log.unfold tr)

  where
  compute m _c to' =
    if ns == Log.toNamespace to' then
      Map.insert (Log.toHostname to') (Log.toTimestamp to') m
    else
      m

------------------------------------------------------------------------------
-- CompletedBlockFetchTimes datapoint: type and boilerplate
--

-- | map 'HostName' to a 'UTCTime'
type HostTimes = Map.Map Network.HostName.HostName UTCTime

-- Create a datapoint

-- | CompletedBlockFetchTimes - for tracking times of this trace object:
--             ["BlockFetch","Client","CompletedBlockFetch"]
newtype CompletedBlockFetchTimes = CompletedBlockFetchTimes HostTimes
                                   deriving (Eq,Ord,Read,Show,Generic)
deriving instance ToJSON CompletedBlockFetchTimes

instance Log.MetaTrace CompletedBlockFetchTimes
  where
  namespaceFor _  = Log.Namespace [] ["BlockFetch","LastCompletedTimes"]
  severityFor _ _ = Just Info
  documentFor _   = Just "map of most recent CompletedBlockFetch times for all connected nodes"
  allNamespaces   = [Log.namespaceFor (undefined :: CompletedBlockFetchTimes)]


---- utilities -----------------------------------------------------

convertTime :: String -> UTCTime
convertTime = parseTimeOrError False defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

slotWhenSlotChangedTo1Sec :: SlotNo
slotWhenSlotChangedTo1Sec = SlotNo 4492800

timeWhenSlotChangedTo1Sec :: UTCTime
timeWhenSlotChangedTo1Sec = convertTime $ "2020-07-29T21:44:51Z"
 -- convertTime $ "2020-07-28T20:20:16Z"
 -- POSIXTime 1595967616000  -- 2020/07/28 20:20:16 - epoch:74 - slot:1598400 - block:1597133  


_systemStart :: UTCTime
_systemStart = convertTime $ "2017-09-23T21:44:51Z"

slotLength :: NominalDiffTime  -- '... will treat it as seconds'
slotLength  = 1

slotStart :: SlotNo -> UTCTime
slotStart =
    flip addUTCTime timeWhenSlotChangedTo1Sec
  . (* slotLength)
  . (\s-> s - (fromIntegral $ unSlotNo slotWhenSlotChangedTo1Sec))
  . fromIntegral
  . unSlotNo

stub :: a
stub = error "stub"
