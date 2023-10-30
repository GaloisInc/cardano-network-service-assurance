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

-- package prometheus:
import qualified System.Metrics.Prometheus.Concurrent.Registry as PR
import qualified System.Metrics.Prometheus.Metric.Counter      as PC

-- local to this pkg:
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
