{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.Tracer.CNSA.Analysis.Catalog.CountTraceLogs.Analysis
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

------------------------------------------------------------------------------
-- a trivial "proof of life" analysis:
--

analysis :: Analysis
analysis =
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
