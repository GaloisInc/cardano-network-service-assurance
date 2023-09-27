# Cardano Node Service Assurance Review

## Notes

`Cardano.Tracer.CNSA.ParseLogs`: it would be nice to have tests for the
decoders, so we can discover early that encoding has changed upstream.

Everything is implement in `IO`.  Is it worth to use `io-classes` (we don't
currently support `IORef`s, but that could be added).  This might help to test
the library.  `IORef` are lazy, if we add strict `IORef`s to the `io-sim`
ecosystem, this project could benefit from them.

### Implemented analyses

* `header` arrival times
* times when a peer requests a block (fetch request).  We only record who
  requests the block but we don't store the sender (is this right? or is this the other way around).
* times when we downloaded a block from a peer (as above we only get the peer
  that requested the block)
* block adoption times,  sizes of blocks and length of the blockchain.

#### Prometheus metrics

* top slot (accross all nodes) - it might to correspond to a seen header rather than a block 
* penultimate slot (accross all nodes) - as above
* delay histogram 

### `Cardano.Tracer.CNSA.CnsaAnayses` module

#### `BlockState`

```
type BlockState = Map Hash BlockData
```

This assumes that there are no `Hash` collications on the chain.  I'd expect
that eventually there will be? If if we use `Point` instead (basically a tuple
of `SlotNo` and `Hash`), we wouldn't have that problem.

####

Can this function
```
mkBlockStatusAnalysis
  :: Trace IO DataPoint
  -> OrigCT.Tracer IO String
  -> PR.Registry
  -> IO (Log.TraceObject -> LO.LOBody -> IO ())
```
be turned into
```
mkBlockStatusAnalysis
  :: Trace IO DataPoint
  -> OrigCT.Tracer IO String
  -> PR.Registry
  -> Log.TraceObject
  -> LO.LOBody
  -> IO ()
```
or if it requires some state that needs to be keept behind the scene the `with`
pattern might be a good idea, e.g.

```
data State = ...

withState :: :: Trace IO DataPoint
             -> OrigCT.Tracer IO String
             -> PR.Registry
             -> (State -> IO a)
             -> IO a
withState = undefined

-- or maybe it should be called 'analyseEvent'
logEvent :: State -> Log.TraceObject -> LO.LOBody -> IO ()
logEvent = undefined
```


####

Should `bl_downloadeedHeader`, `bl_sendFetchrequest`, `bl_completdBlockFetch`
be strict maps?

Quite likely all records should be strict as well.

It would be nice to add haddocks to each of these fields and describe how they
are gathered.

```
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
```

### `runCSNASink`

At some stage more `TracerConfig` entries should be exposed in options, e.g.
`networkMagic` (to monitor testnets).


### Future work

Currently all data is stored in an `IORef`.  The data will grow with
blockchain.  The more nodes the service tracks the faster it will grow, so
eventually this data will be needed to be stored in a data base.

###

There are various FIXMEs in the code.  Should they be turned into issues?

# trace-forward

It should use `strict-stm` package.

```haskell
-- | Since 'DataPointForward' protocol does not assume the stream of requests/replies,
--   we use the 'TVar's to provide to acceptor's side an ability to ask 'DataPoint's
--   explicitly.
data DataPointRequestor = DataPointRequestor
  { -- | The "ask flag": we use it to notify that we want 'DataPoint's.
    askDataPoints   :: !(TVar Bool)
    -- | The names of 'DataPoint's we need.
  , dataPointsNames :: !(TVar [DataPointName])
    -- | The list of received 'DataPoint's' values.
    --   By default it's empty, but when 'DataPoint's
    --   are received they will be stored here.
  , dataPointsReply :: !(TMVar DataPointValues)
  }
```

## `TraceEnv` smart constructor

`CNSA` contains this code.  Is it similar in other part of the system? If so it
would be good to wrap this in a single record.

```
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
```

which later constructs `TraceEnv` from these calls.  Consider adding a function
which does exactly that.

Some of the interfaces expose `TVar`'s, if one side only reads and another
(possibly another library) only supply the values (or vice versa) it's good to
expose `STM` actions instead.
