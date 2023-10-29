module Cardano.Tracer.CNSA.Analysis.Catalog
  ( analyses
  )
where

import           Cardano.Tracer.CNSA.Analysis.Types
import qualified Cardano.Tracer.CNSA.Analysis.Catalog.BlockState.Analysis
import qualified Cardano.Tracer.CNSA.Analysis.Catalog.CountTraceLogs.Analysis
import qualified Cardano.Tracer.CNSA.Analysis.Catalog.SamplerBandwidth.Analysis


------------------------------------------------------------------------------
-- All CNSA Analyses:

analyses :: [Analysis]
analyses =
  [ Cardano.Tracer.CNSA.Analysis.Catalog.BlockState.Analysis.analysis
  , Cardano.Tracer.CNSA.Analysis.Catalog.CountTraceLogs.Analysis.analysis
  , Cardano.Tracer.CNSA.Analysis.Catalog.SamplerBandwidth.Analysis.analysis
  ]
