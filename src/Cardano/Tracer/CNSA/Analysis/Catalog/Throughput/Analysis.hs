{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.Tracer.CNSA.Analysis.Catalog.Throughput.Analysis
  ( analysis
  )
where

-- base:
import           Data.IORef
import qualified Data.Map as Map
import qualified Data.Text as T
import           Data.Time (UTCTime)

-- package contra-tracer: (not to be confused with Cardano.Tracer....)
import qualified "contra-tracer" Control.Tracer as OrigCT

-- cardano packages:
import qualified Cardano.Logging.Types as Log

-- package locli: (or slice thereof)
import qualified Cardano.Unlog.LogObject as LO

-- package prometheus:
import qualified System.Metrics.Prometheus.Concurrent.Registry as PR
import qualified System.Metrics.Prometheus.Metric.Counter      as PR
import qualified System.Metrics.Prometheus.MetricId            as PR

-- local to this pkg:
import           Cardano.Tracer.CNSA.Analysis.Types
import           Cardano.Tracer.CNSA.Run.ParseLogs
import           Cardano.Utils.Log

------------------------------------------------------------------------------
-- block fetch throughput analysis for each sampler:
--  - just captures total number of bytes
--    (samplers of this would compute throughput)
--

analysis :: Analysis
analysis =
  Analysis{ aName = "block fetch total bytes"
          , aTraceNamespaces = NoFilter
          , aInitialize = initialize
          , aProcessTraceObject = procTrObjBlockFetchThroughputAnalysis
          }
  where
    initialize _args = do
      s <- newIORef (Map.empty :: State)
      return (Right s)

type State = Map.Map Sampler SmplrState

data SmplrState = SS {byteCounter :: PR.Counter, timeStamp :: UTCTime}

-- FIXME: add a datapoint:
-- type DataPoint = Map.Map Sampler (Int, UTCTime)

procTrObjBlockFetchThroughputAnalysis
  :: AnalysisArgs
  -> IORef State
  -> Log.TraceObject -> LO.LOBody
  -> IO ()
procTrObjBlockFetchThroughputAnalysis args st trObj logObj =
  case logObj of
    LO.LOBlockFetchClientCompletedFetch hash ->
      case getFieldFromTraceObject' ["size"] trObj of
        Left s   -> warnMsg ["in BlockFetch.Client.Completed.Fetch:", s]
        Right sz -> processReceivedBlockSize hash sz
    _ -> return ()

  where
    time  = Log.toTimestamp trObj
    shost = Log.toHostname  trObj -- the sampler host

    myDebugTrace = OrigCT.traceWith $ aaDebugTr args

    processReceivedBlockSize hash blockSize = do

      -- update your sampler map: new key?, always update time
      modifyIORefIO st $ \m-> do
        case m Map.!? shost of
          Just _sd ->
              return $ Map.adjust (\sd->sd{timeStamp=time}) shost m
          Nothing -> do
              cntr <- PR.registerCounter
                        "downloaded_blocks_total_bytes"
                        (PR.addLabel "host" (T.pack shost) mempty)
                        (aaRegistry args)
              return $ Map.insert shost (SS cntr time) m

      -- update prometheus counter associated with shost:
      ss <- (Map.! shost) <$> readIORef st
      PR.add blockSize (byteCounter ss)
      myDebugTrace $ unwords
        ["block size:", shost, show hash, "  size:  ", show blockSize]


---- IORef utilities -------------------------------------------------

modifyIORefIO :: IORef a -> (a -> IO a) -> IO ()
modifyIORefIO ref f =
  do
    a  <- readIORef ref
    a' <- f a
    writeIORef ref a'
  -- NOTE: not re-entrant
