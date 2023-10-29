# Spinning Up and Using Cardano Network Service Assurance (CNSA)
## Our Instantiation

Our *demonstration* instantiation of CNSA includes
  - Two sampling nodes `$SA1_HOST` `$SA2_HOST`, each of these will be
    running
     - `cardano-node` configured with new-tracing.
     - `cardano-tracer` configured to re-forward the tracing mini-protocols.
    
  - One sink node `$SINK_HOST` running `cnsa-sink`.

The required configuration files can be found in 
[the configuration directory](../config/).
Beyond the configuration files, we capture shared system information in
[variables.sh](../scripts/variables.sh):
- You will need to update and extend this for your needs.  In
  particular you will need to change the `*_HOST` and `*_IPADDR` variables,
  but you should be able to leave the `*_SOCK` variables alone. Note the
  documentation in the shell file.
- You'll need to `source` this file on all your nodes.

This configuration will give two sampling nodes, each with 20 hot
peers, running in P2P mode.

## Extra installs

Port forwarding is part of the CNSA design: continuing to follow the
"principle of least privilege" as is currently reflected in the
new-tracing system of cardano-node.
It is highly recommended to use `autossh` for port forwarding
(rather than plain `ssh`), it is a far more robust solution.
Install `autossh` on `$SINK_NODE`:

    $ sudo apt install autossh

In the following we use `tmux` for convenience; install it with

    $ sudo apt install tmux

CNSA optionally uses InfluxDB to store block data that overflows its internal
buffer. It will run without an InfluxDB installation, but to take advantage of
the database capabilities, you'll need to install InfluxDB.

On Mac, with homebrew:
    
    $ brew install influxdb-cli
    $ brew install influxdb@1

On Linux:

    $ wget https://dl.influxdata.com/influxdb/releases/influxdb-1.8.10_linux_amd64.tar.gz
    $ tar xvfz influxdb-1.8.10_linux_amd64.tar.gz


## System Setup & Configuration

Every sampling node 
 - needs the config files
 - should have the shell variables defined 

We use `rsync` to copy the config files from your local host to the
sampling nodes:

    CNSA=~/src/iog/cardano-network-service-assurance  # your repo
    source ${CNSA}/scripts/variables.sh
    ALLSAMPLINGHOSTS="${SA1_HOST} ${SA2_HOST}"
    for host in ${ALLSAMPLINGHOSTS}; do
      rsync -plv ${CNSA}/config/* $host:${NODE_CFG_RELDIR}/
    done

## Spinning Up CNSA
### Sampling Nodes

For each sampling node, you'll start `cardano-node` and
`cardano-tracer`; the order doesn't matter and each process is
resilient to the other process starting and stopping.  On your
`$SA1_HOST` sampling node:

     ssh $SA1_HOST
     source $CNSA/scripts/variables.sh  # or from ...

Start `cardano-node`:

     tmux new -s cardano-node

      MYIP=$SA1_IPADDR
      cardano-node run \
       --config $NODE_CFG_DIR/config-p2p.yaml \
       --database-path $NODE_CFG_DIR/db/ \
       --socket-path $CARDANO_NODE_SOCKET_PATH \
       --host-addr $MYIP \
       --port 1337 \
       --topology $NODE_CFG_DIR/topology-p2p.json \
       --tracer-socket-path-accept $TRACER_SOCK

And start `cardano-tracer`, configured with "re-forwarding":

     tmux new -s cardano-tracer

      cardano-tracer --config $NODE_CFG_DIR/c-tracer-config-reforwarding.yaml

Note that our configurations are setup for P2P, this is not a
requirement.

A couple notes regarding `config/c-tracer-config-reforwarding.yaml`:

- The configuration data duplicates two variables in
  [variables.sh](../scripts/variables.sh): 

        TRACER_SOCK=/tmp/cardano-node-to-cardano-tracer.sock
        REFWD_SOCK=/tmp/cardano-tracer-reforwarding.sock

- Note the configuration of "re-forwarding"
   ```yaml
   hasForwarding:
     - network:
       tag: AcceptAt
       contents: "/tmp/reforwarder.sock"
     - [["BlockFetch","Client"],
        ["ChainSync","Client"],
        ["ChainDB","AddBlockEvent"]
       ]
   ```
   This will enable the "re-forwarding" which serves the tracing
   protocol on the Unix socket `/tmp/reforwarder.sock` and only 
   re-forwards log messages (having a hierarchical namespace) that
   have one of the indicated prefixes.  Currently the three prefixes
   are sufficient to compute CNSA's current "analyses" (see below).
   
### Sink Node (`cnsa-sink`)

The following is executed on the `$SINK_HOST`:

    ssh $SINK_HOST
    source $CNSA/scripts/variables.sh  # or from ...

First, forward ports from all your sampling nodes to the sink host:

    tmux new -s port-forwarding
    
     [ -e $SA1_REMOTE_TRACER_SOCK ] && unlink $SA1_REMOTE_TRACER_SOCK
     [ -e $SA2_REMOTE_TRACER_SOCK ] && unlink $SA2_REMOTE_TRACER_SOCK

     autossh -nNT -M 0 -o "ExitOnForwardFailure yes" \
       -L $SA1_REMOTE_TRACER_SOCK:$REFWD_SOCK $SA1_HOST &
     autossh -nNT -M 0 -o "ExitOnForwardFailure yes" \
       -L $SA2_REMOTE_TRACER_SOCK:$REFWD_SOCK $SA2_HOST &

Note that we are connecting *not* to the `cardano-node` tracing socket, but to
the new socket re-forwarded by `cardano-tracer`.

Second, start InfluxDB.

On Mac:
    brew services start influxdb@1
    
On Linux:
    tmux new -s influxdb

     /path/to/influxdb-1.8.10-1/usr/bin/influxd

Third, start `cnsa-sink`

    tmux new -s sink
    
     cnsa-sink $SA1_REMOTE_TRACER_SOCK $SA2_REMOTE_TRACER_SOCK

You'll see a pretty noisy `stdout` which includes debugging information
and various warnings.  More "info level" logging messages are prefixed
with "Overflow:", see *The `CNSA.BlockState` Datapoint and Log* below.

## Exercising the `cnsa-sink` service
### Storage of filtered, forwarded logs

Note that the *filtered* log from each sampling node is being 
stored on the sink node:

    ls -rlt /tmp/cnsa-sink-m-logs/tmp-cnsa-trace-forward-sa*sock/

### Serving System Level Datapoints

We can verify that `cnsa-sink` is serving the `CNSA.BlockState` Datapoint

    tmux new -s demo
    
     demo-acceptor $SINKSERVER_SOCK Initiator CNSA.BlockState

`demo-acceptor` polls the *New Tracing Protocol* on `$SINKSERVER_SOCK`,
requests the `CNSA.BlockState` Datapoint, and prints
the result as JSON.

### The `CNSA.BlockState` Datapoint and Log

Currently CNSA supports one Datapoint, `CNSA.BlockState`, its Haskell
type is `BlockState` defined in [the source
code](../src/Cardano/Tracer/CNSA/CnsaAnalyses.hs) as follows:

    type BlockState = Map Hash BlockData
    data BlockData =
      BlockData { bl_blockNo             :: BlockNo
                , bl_slot                :: SlotNo
                , bl_downloadedHeader    :: [(Peer,UTCTime)]
                , bl_sendFetchRequest    :: [(Peer,UTCTime)]
                , bl_completedBlockFetch :: [(Peer,UTCTime)]
                , bl_addedToCurrentChain :: Maybe UTCTime
                , bl_size                :: Maybe Int
                }

As currently configured, the `Map` holds the five most recent blocks
(as ordered by `SlotNo`), this data can be queried anytime via the 
Datapoint protocol, as we show above.

As BlockData is rotated out of the `Map` it is logged to `stdout` prefixed
with "Overflow:", e.g.,

    Overflow: (473f6b792214d4cafe06be1d2f0e14b5c4ab3eaa4dc1ef02de1628b89d3e116a,BlockData {bl_blockNo = BlockNo 9056351, bl_slot = SlotNo 98348687, bl_downloadedHeader = [("54.248.146.238:6000",2023-07-21 04:49:38.205928177 UTC),("34.83.231.227:6001",2023-07-21 04:49:38.339494503 UTC),("54.64.243.69:1338",2023-07-21 04:49:38.179543193 UTC),...], bl_sendFetchRequest = [("3.222.153.137:3001",2023-07-21 04:48:44.132977085 UTC),("3.216.77.109:3001",2023-07-21 04:48:44.128199831 UTC)], bl_completedBlockFetch = [("3.216.77.109:3001",2023-07-21 04:48:44.221206052 UTC),("3.222.153.137:3001",2023-07-21 04:48:44.230211113 UTC)], bl_addedToCurrentChain = Just 2023-07-21 04:48:44.314005457 UTC, bl_size = Nothing})

Thus, the following invocation of `cnsa-sink` allows us to capture
all `BlockData` as it is rotated out of the `BlockState` map:

    cnsa-sink $SA1_REMOTE_TRACER_SOCK $SA2_REMOTE_TRACER_SOCK \
      | tee >(grep "^Overflow:" > ~/overflow-blockdata.log)

Future work:

> Add a more powerful and structured logging system, in particular as
> we add further types of log messages.

### Serving Prometheus Metrics

The `cnsa-sink` process serves prometheus metrics at this URL:
http://localhost:8080/metrics.  It serves all three types of metrics:
counters, histograms, and gauges.  Note the following output from
`curl http://localhost:8080/metrics`:

    # TYPE count_of_tracelogs counter
    count_of_tracelogs  21045
    # TYPE propDelays histogram
    propDelays_bucket{le="0.1"} 822.0
    propDelays_bucket{le="0.2"} 30709.0
    propDelays_bucket{le="0.30000000000000004"} 117186.0
    propDelays_bucket{le="0.4"} 205255.0
    propDelays_bucket{le="0.5"} 277477.0
    propDelays_bucket{le="0.6"} 350158.0
    propDelays_bucket{le="0.7000000000000001"} 412334.0
    propDelays_bucket{le="0.8"} 456069.0
    propDelays_bucket{le="0.9"} 483936.0
    propDelays_bucket{le="1.0"} 503337.0
    propDelays_bucket{le="1.1"} 518777.0
    propDelays_bucket{le="1.2000000000000002"} 529779.0
    propDelays_bucket{le="1.3000000000000003"} 536390.0
    propDelays_bucket{le="1.4000000000000001"} 541698.0
    propDelays_bucket{le="1.5000000000000002"} 545240.0
    propDelays_bucket{le="1.6"} 549273.0
    propDelays_bucket{le="1.7000000000000002"} 552278.0
    propDelays_bucket{le="1.8000000000000003"} 554839.0
    propDelays_bucket{le="1.9000000000000001"} 556219.0
    propDelays_bucket{le="2.0"} 557565.0
    propDelays_bucket{le="+Inf"} 578843.0
    propDelays_sum  484713.4123378238
    propDelays_count  578843
    # TYPE slot_penultimate gauge
    slot_penultimate  9.833154e7
    # TYPE slot_top gauge
    slot_top  9.8331553e7

where
  - `count_of_tracelogs`, of type `counter`, is counting the
    log messages received from the sampling nodes; it's not too useful
    but can serve as a "proof of life."
  - `propDelays`, of type `histogram`, is recording for all peers, 
    the time since slot start (delay) at which the sampling node
    learns that a peer sees a new header.
  - `slot_top`, of type `gauge`, is the highest slot number for the blocks
    seen so far.
  - `slot_penultimate`, of type `gauge`, is the second highest slot number
    for the blocks seen so far.

### Retrieving Stored Block Data

#### In Haskell

`Cardano.Tracer.CNSA.BlockState.DB` exports `readBlockData`, which will read all
entries in the database and decode their JSON bodies into Haskell `BlockData`
objects. The `delay-demo` application acts as an example client of this
functionality, reading from the database and applying an aggregation analysis
over the data. In particular, it calculates the average delay between each
slot's inherent time and the time when each peer sends each sampler a header for
a block in that slot.

You can run the application like so:
    
    cabal run delay-demo

This will write two JSON files. `block-data.json` contains the JSON encoding of
the entries in the database, keyed by hash, and `delays.json` contains the
result of the delay analysis.

#### From InfluxDB Shell

You can also see the "raw" block data entries. Enter the InfluxDB shell:

On Mac:
    
    $ influx v1 shell

On Linux:
    
    $ influx

From the InfluxDB shell, you can list the block data that it has stored:

    > use blocks
    > SELECT * FROM "block-data"

At present, the block data is largely encoded as JSON. Haskell applications are
well-poised to decode this JSON through the derived `FromJSON` instances of
`BlockData` and its subsidiaries.

