{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.CNSA.CnsaSink
  ( runCnsaSink
  ) where

import           Control.Concurrent.Async.Extra (sequenceConcurrently)
import           Control.Concurrent.Extra (newLock)
import           Control.Concurrent.STM.TVar (newTVarIO)
import           Control.Monad (void)
import qualified Data.List.NonEmpty as NE

import qualified Cardano.Logging.Types as Log
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Acceptors.Run
import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.ReForwarder
import           Cardano.Tracer.Handlers.RTView.Run
import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.MetaTrace
import           Cardano.Tracer.Utils

import           Cardano.Tracer.CNSA.CnsaAnalyses (mkCnsaSinkAnalyses)


runCnsaSink :: [FilePath] -> IO ()
runCnsaSink localSocks = do
  protocolsBrake <- initProtocolsBrake
  dpRequestors <- initDataPointRequestors
  connectedNodes <- initConnectedNodes
  connectedNodesNames <- initConnectedNodesNames
  acceptedMetrics <- initAcceptedMetrics
  savedTO <- initSavedTraceObjects
  chainHistory <- initBlockchainHistory
  resourcesHistory <- initResourcesHistory
  txHistory <- initTransactionsHistory
  currentLogLock <- newLock
  currentDPLock <- newLock
  eventsQueues <- initEventsQueues Nothing connectedNodesNames dpRequestors currentDPLock

  rtViewPageOpened <- newTVarIO False
  tr <- mkTracerTracer $ SeverityF $ Just Warning
  (reforwardTraceObject,trDataPoint) <- initReForwarder config tr

  -- cnsaSinkTraceFunc :: Log.TraceObject -> IO ()
  (cnsaSinkTraceFunc,prometheus) <- mkCnsaSinkAnalyses trDataPoint
    
  let reforwardTraceObject' os = do
                                 reforwardTraceObject os
                                 mapM_ cnsaSinkTraceFunc os
      
  let tracerEnv =
        TracerEnv
          { teConfig                = config
          , teConnectedNodes        = connectedNodes
          , teConnectedNodesNames   = connectedNodesNames
          , teAcceptedMetrics       = acceptedMetrics
          , teSavedTO               = savedTO
          , teBlockchainHistory     = chainHistory
          , teResourcesHistory      = resourcesHistory
          , teTxHistory             = txHistory
          , teCurrentLogLock        = currentLogLock
          , teCurrentDPLock         = currentDPLock
          , teEventsQueues          = eventsQueues
          , teDPRequestors          = dpRequestors
          , teProtocolsBrake        = protocolsBrake
          , teRTViewPageOpened      = rtViewPageOpened
          , teRTViewStateDir        = Nothing
          , teTracer                = tr
          , teReforwardTraceObjects = reforwardTraceObject'
          }
  void . sequenceConcurrently $
    [ runAcceptors tracerEnv
    , prometheus
    ]
 where
  config = TracerConfig
    { networkMagic   = 764824073
    , network        = ConnectTo $ NE.fromList $ map LocalSocket localSocks
    , loRequestNum   = Just 1
    , ekgRequestFreq = Just 1.0
    , hasEKG         = Nothing
    , hasPrometheus  = Nothing
    , hasRTView      = Nothing
    , logging        = NE.fromList
                       [LoggingParams "/tmp/cnsa-sink-m-logs"
                                       -- FIXME
                                      FileMode ForMachine]
    , rotation       = Nothing
    , verbosity      = Just Minimum
    , metricsComp    = Nothing
    , hasForwarding  = Just ( AcceptAt
                                (LocalSocket "/tmp/cnsa-sink.sock")
                                             -- FIXME
                            , Just [] -- reforwards no logObjects
                            , Log.defaultForwarder
                            )
    }


