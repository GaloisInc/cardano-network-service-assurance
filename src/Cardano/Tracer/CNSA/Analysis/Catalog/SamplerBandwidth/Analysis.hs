{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.Tracer.CNSA.Analysis.Catalog.SamplerBandwidth.Analysis
  ( analysis
  )
where

-- base:
import qualified Data.Map as Map
import           Data.Time (NominalDiffTime)

-- package contra-tracer: (not to be confused with Cardano.Tracer....)
import qualified "contra-tracer" Control.Tracer as OrigCT

-- cardano packages:
import qualified Cardano.Logging.Types as Log

-- package locli: (or slice thereof)
import qualified Cardano.Unlog.LogObject as LO

-- package prometheus:
import qualified System.Metrics.Prometheus.Concurrent.Registry as PR
import qualified System.Metrics.Prometheus.Metric.Counter      as PC

-- local to this pkg:
-- FIXME1:
import           Cardano.Tracer.CNSA.Analysis.Types
import           Cardano.Tracer.CNSA.Run.ParseLogs
import           Cardano.Utils.Log

------------------------------------------------------------------------------
-- block fetch bandwidth analysis for each sampler
--

analysis :: Analysis
analysis =
  Analysis{ aName = "block fetch bandwidth"
          , aTraceNamespaces = NoFilter
          , aInitialize = initialize
          , aProcessTraceObject = procTrObjBlockFetchBandwidthAnalysis
          }
  where

    initialize args = do
      metric_byteCtr <-
        PR.registerCounter "blockbytes_downloaded" mempty (aaRegistry args)
      return (Right metric_byteCtr)

procTrObjBlockFetchBandwidthAnalysis
  :: AnalysisArgs
  -> PC.Counter
  -> Log.TraceObject -> LO.LOBody -> IO ()
procTrObjBlockFetchBandwidthAnalysis aArgs metric_byteCtr trObj logObj =
  -- FIXME: change to be compute "per sampler".
  case logObj of
    LO.LOBlockFetchClientCompletedFetch hash ->
      case getFieldFromTraceObject' ["size"] trObj of
        Left s   -> warnMsg ["in BlockFetch.Client.Completed.Fetch:", s]
        Right sz -> receivedBlockSize hash sz
    _ -> return ()

  where
    time  = Log.toTimestamp trObj
    shost = Log.toHostname  trObj -- the sampler host

    debugTr = aaDebugTr aArgs

    receivedBlockSize hash blockSize =
      do
      PC.add blockSize metric_byteCtr
      OrigCT.traceWith debugTr $ unwords
        ["block size:", shost, show hash, "  size:  ", show blockSize]
