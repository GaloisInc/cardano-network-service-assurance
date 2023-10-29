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
-- FIXME1:
import           Cardano.Tracer.CNSA.Analysis.Types
import           Cardano.Tracer.CNSA.Run.ParseLogs
import           Cardano.Utils.Log
import           Cardano.Utils.SlotTimes

------------------------------------------------------------------------------
-- block fetch bandwidth analysis:
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
  --   FIXME: create abstractions over "sampler separated" analyses
  case logObj of
    LO.LOBlockFetchClientCompletedFetch hash ->
      case getFieldFromTraceObject' ["size"] trObj of
        Left s   -> warnMsg ["in BlockFetch.Client.Completed.Fetch:", s]
        Right sz -> receivedBlockSize hash sz
    _ -> return ()

  where
    debugTr = aaDebugTr aArgs
    receivedBlockSize hash blockSize =
      do
      PC.add blockSize metric_byteCtr
      OrigCT.traceWith debugTr $ unwords
        ["block size:", shost, show hash, "  size:  ", show blockSize]

    -- time  = Log.toTimestamp trObj
    shost = Log.toHostname  trObj -- the sampler host
