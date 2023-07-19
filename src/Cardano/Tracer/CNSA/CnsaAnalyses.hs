{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
  -- FIXME: delete ^
    
module Cardano.Tracer.CNSA.CnsaAnalyses
 ( mkCnsaSinkAnalyses
 )
where

-- base:
import           Control.Monad
import           Data.IORef
import           Data.List (sortOn)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict(Map)
import           Data.Time (nominalDiffTimeToSeconds,diffUTCTime,UTCTime)

-- package contra-tracer: (not to be confused with Cardano.Tracer....)
import qualified "contra-tracer" Control.Tracer as CT
  
-- cardano packages:
import qualified Cardano.Logging.Types as Log
import           Cardano.Slotting.Block
import           Cardano.Slotting.Slot
import           Cardano.Tracer.MetaTrace hiding (traceWith)
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
import           Cardano.Tracer.CNSA.ParseLogs
import           Cardano.Utils.SlotTimes


------------------------------------------------------------------------------
-- Create *all* CNSA Sink Analyses
--

mkCnsaSinkAnalyses :: Trace IO DataPoint
                   -> CT.Tracer IO String
                   -> IO (Log.TraceObject -> IO (), IO ())
mkCnsaSinkAnalyses traceDP debugTr =
  do
  registry <- PR.new

  -- trivial 'proof of life' metric:
  metric_traceCtr <- PR.registerCounter "count_of_tracelogs" mempty registry

  sendTraceLogObj <- mkBlockStatusAnalysis traceDP debugTr registry
  return (
           -- handle TraceObject:
           \trObj->
             do
             PC.inc metric_traceCtr
             
             CT.traceWith debugTr ("recvd traceLogObj: " ++ show trObj) 
             case getLogBody trObj of
               Left s -> warn
                           [ "unparseable TraceObject:", s]
               Right lb ->
                   do
                   CT.traceWith debugTr ("parsed log body: " ++ show lb)
                   case lb of
                     LB_LOBody l -> sendTraceLogObj trObj l
                     LB_Etc    _ -> return ()

         , PS.serveMetrics 8080 ["metrics"] (PR.sample registry)
             -- http://localhost:8080/metrics
             -- FIXME: configure port
         )


------------------------------------------------------------------------------
-- Code for Analysis: mkBlockStatusAnalysis
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

blockStateMax :: Int
blockStateMax = 5
  -- FIXME: update to 10 ?
  -- FIXME: turn into CL parameter.

mkBlockStatusAnalysis
  :: Trace IO DataPoint
  -> CT.Tracer IO String
  -> PR.Registry
  -> IO (Log.TraceObject -> LO.LOBody -> IO ())
mkBlockStatusAnalysis _traceDP debugTr registry =
  do
  blockStateRef   <- newIORef Map.empty
  topSlotGauge    <- PR.registerGauge     "slot_top"         mempty registry
  penultSlotGauge <- PR.registerGauge     "slot_penultimate" mempty registry
  propDelaysHist  <- PR.registerHistogram "propDelays"       mempty
                       [0.1,0.2..2.0]
                       registry

  let
    doLogEvent :: Log.TraceObject -> LO.LOBody -> IO ()
    doLogEvent trObj logObj =
      do
      let time = Log.toTimestamp trObj
          withPeer f =
              case getPeerFromTraceObject trObj of
                Left s  -> warn ["expected peer, ignoring trace: " ++ s]
                              
                Right p -> f p
          host = Log.toHostname trObj
          updateBlockData f = modifyIORef' blockStateRef f
          updateBlockDataByKey k f =
            modifyIORefMaybe blockStateRef
              (\m-> case adjustIfMember f k m of
                      Just m' -> return (Just m')
                      Nothing ->
                        do
                        warn 
                          ["ignoring Log, hash not in current block data: "
                           ++ show k]
                        return Nothing
              )         
          
      -- Update 'blockStateRef :: IORef BlockState' :
      case logObj of 
        LO.LOChainSyncClientSeenHeader slotno blockno hash ->
            withPeer $ \peer->
              updateBlockData (addSeenHeader slotno blockno hash peer time)

        LO.LOBlockFetchClientRequested hash len ->
            withPeer $ \peer->
              updateBlockData (addFetchRequest hash len peer time)
    
        LO.LOBlockFetchClientCompletedFetch hash ->
            withPeer $ \peer->
              updateBlockDataByKey hash (addFetchCompleted hash peer time)

        LO.LOBlockAddedToCurrentChain hash msize len ->
            updateBlockData (addAddedToCurrent hash msize len host time)

        _ ->
            return ()
            
      let getSortedByKeys m =
              reverse
            $ sortOn (bl_slot . snd)
            $ Map.toList m

          debugTraceBlockData nm es =
            do
            CT.traceWith debugTr (nm ++ " block data:")
            mapM_ (CT.traceWith debugTr . show) es
            CT.traceWith debugTr ""
            -- FIXME[F3]: make fancier
            
      -- debugging:
      () <- do
            raw0 <- readIORef blockStateRef
            debugTraceBlockData "blockState[pre]" (getSortedByKeys raw0)

      -- update 'blockStateRef', removing overflow:
      overflowList <- atomicModifyIORef'
                        blockStateRef
                        (splitMapOn blockStateMax bl_slot)

      -- Process 'overflowList': (print to stdout for now) [FIXME]
      if null overflowList then
        CT.traceWith debugTr ("Overflow: none")
      else
        mapM_ (\b-> putStrLn ("Overflow: " ++ show b))
              overflowList

      -- Update Metrics:
      raw1 <- getSortedByKeys <$> readIORef blockStateRef
      debugTraceBlockData "blockState[post]" raw1
      case map snd raw1 of
        b0:b1:_ ->
            do
            let
              SlotNo slot0 = bl_slot b0
              SlotNo slot1 = bl_slot b1

            -- update slot metrics:
            PG.set (fromIntegral slot0) topSlotGauge
            PG.set (fromIntegral slot1) penultSlotGauge
              -- NOTE: No integers? everything a float?!
              --       See node's prometheus output: has integers

            -- update propagation metrics for b1/slot1 (penultimate):
            let
              cvtTime = fromRational . toRational . nominalDiffTimeToSeconds
              delays  = map 
                          (\(p,t)->
                             (p, diffUTCTime t (slotStart (SlotNo slot1))))
                          (bl_downloadedHeader b1)
            CT.traceWith debugTr $ unwords ["slot_top:", show slot0]
            CT.traceWith debugTr $ unwords ["slot_pen:", show slot1]
            CT.traceWith debugTr $ unwords ["delays:"  , show delays]
            when (any (\(_,d)-> d < 0) delays) $
              putStrLn "ERROR: NEGATIVE DELAY"
            mapM_ (\v-> PH.observe v propDelaysHist)
                  (map (cvtTime . snd) delays)

        -- unless we have at least two blocks recorded, do nothing:
        _ ->
            return ()
              
  return doLogEvent

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

addFetchRequest _hash _len peer time m = m
  {-
  ---
  Warn if not in map.
  Map.insertWith
    (\_ o->o{bl_sendFetchRequest= bl_sendFetchRequest o ++ [(peer,time)]})
    hash
    (defaultBlockData block slot){bl_downloadedHeader=[(peer,time)]}
  -}
  
addFetchCompleted _hash _peer _time m = m

addAddedToCurrent _hash _msize _len _host _time m = m


---- Map utilities -------------------------------------------------

-- | splitMapOn n f m -
--     reduce size of Map m to n elements, use f to order element values.

splitMapOn :: (Ord k, Ord b)
           => Int -> (a -> b) -> Map k a -> (Map k a, [(k,a)])
splitMapOn n f m0 = (Map.fromList keepers, overflow)
  where
  (keepers,overflow) = splitAt n $ reverse $ sortOn (f . snd) $ Map.toList m0

adjustIfMember :: Ord k => (a -> a) -> k -> Map k a -> Maybe (Map k a)
adjustIfMember f k m =
  if k `Map.member` m then
    Just $ Map.adjust f k m
  else
    Nothing
  -- not seeing how to do with single call.


---- IORef utilities -------------------------------------------------

modifyIORefMaybe :: IORef a -> (a -> IO (Maybe a)) -> IO ()
modifyIORefMaybe ref f =
  do
  a <- readIORef ref
  ma <- f a
  case ma of
    Just a' -> writeIORef ref a'
    Nothing -> return ()

---- Etc utilities -------------------------------------------------

-- FIXME: make more configurable/?
warn :: [String] -> IO ()
warn []     = return ()
warn (s:ss) = mapM_ putStrLn $ ("Warning: "++s) : map ("  "++) ss

---- Testing -------------------------------------------------------

test1 =
  do
  let input' = Map.fromList
             $ [(5-s, defaultBlockData 0 (SlotNo s)) | s <- [0..4]]
  blockStateRef <- newIORef input'
  putStrLn "blockState [0]:"
  () <- do
        raw0 <- readIORef blockStateRef
        mapM_ print $ reverse $ sortOn (bl_slot . snd) $ Map.toList raw0
  overflowList <- atomicModifyIORef'
                    blockStateRef
                    (splitMapOn 4 bl_slot)
  putStrLn "overflow:"
  mapM_ print overflowList

  raw <- readIORef blockStateRef
  let raw' = reverse $ sortOn (bl_slot . snd)  $ Map.toList raw

  -- Debugging:
  putStrLn "blockState:"
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
              
