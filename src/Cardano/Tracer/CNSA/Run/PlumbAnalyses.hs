{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.Tracer.CNSA.Run.PlumbAnalyses
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
import qualified Cardano.Logging.Types as Log
import           Cardano.Tracer.MetaTrace -- hiding (traceWith)
import           Trace.Forward.Utils.DataPoint

-- package locli: (or slice thereof)
import qualified Cardano.Unlog.LogObject as LO
import           Cardano.Analysis.API.Ground ()

-- package prometheus:
import qualified System.Metrics.Prometheus.Concurrent.Registry as PR
import           System.Metrics.Prometheus.Http.Scrape         as PS

-- local to this pkg:
import           Cardano.Tracer.CNSA.Analysis.Types
import           Cardano.Tracer.CNSA.Analysis.Catalog (analyses)
import           Cardano.Tracer.CNSA.Run.ParseLogs
import           Cardano.Utils.Log

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
