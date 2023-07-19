{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Cardano.Tracer.CNSA.CnsaSink
  ( runCnsaSink
  )
where

-- base:
import           Control.Concurrent.Async.Extra (sequenceConcurrently)
import           Control.Concurrent.Extra (newLock)
import           Control.Concurrent.STM.TVar (newTVarIO)
import           Control.Monad (void)
import qualified Data.List.NonEmpty as NE
import           System.IO (hFlush, stdout)      

-- package contra-tracer: (not to be confused with Cardano.Tracer....)
import qualified "contra-tracer" Control.Tracer as CT
  
-- cardano packages:
import qualified Cardano.Logging.Types as Log
import           Cardano.Tracer.Environment
import           Cardano.Tracer.Acceptors.Run
import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.Logs.Rotator
import           Cardano.Tracer.Handlers.ReForwarder
import           Cardano.Tracer.Handlers.RTView.Run
import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.MetaTrace
import           Cardano.Tracer.Utils

-- local:
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
  (cnsaSinkTraceFunc,prometheus) <-
     mkCnsaSinkAnalyses
       trDataPoint
       (CT.contramap ("DBG: "++) CT.stdoutTracer)
    
  let reforwardTraceObject' os = do
                                 reforwardTraceObject os
                                 mapM_ cnsaSinkTraceFunc os
                                 hFlush stdout
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
    [ runAcceptors   tracerEnv
    , runLogsRotator tracerEnv
    , prometheus
    ]
 where
  config = TracerConfig
    { networkMagic   = 764824073
    , network        = ConnectTo $ NE.fromList $ map LocalSocket localSocks
    , loRequestNum   = Just 1000
                       -- Q. any reason this shouldn't be very large??
    , ekgRequestFreq = Just 1.0
    , hasEKG         = Nothing
    , hasPrometheus  = Nothing
    , hasRTView      = Nothing
    , logging        = NE.fromList
                       [LoggingParams "/tmp/cnsa-sink-m-logs"
                                       -- FIXME: chg w/ CL arg.
                                      FileMode
                                      ForMachine
                       ]
    , rotation       = Just
                         (RotationParams
                           {rpFrequencySecs=    1200    -- 20 mins
                           ,rpLogLimitBytes= 3000000
                           ,rpMaxAgeHours  =       5
                           ,rpKeepFilesNum =      20
                           })
                           
    , verbosity      = Just Minimum
    , metricsComp    = Nothing
    , hasForwarding  = Just ( AcceptAt
                                (LocalSocket "/tmp/cnsa-sink.sock")
                                             -- FIXME: chg w/ CL arg.
                            , Just [] -- reforwards no logObjects
                            , Log.defaultForwarder
                            )
    }


