# Spinning Up and Using Cardano Network Service Assurance (CNSA)
## Our Instantiation

Our demonstration instantiation of CNSA includes
  - two sampling nodes `$SA1_HOST` `$SA2_HOST`, each of these will be
    running
     - `cardano-node` configured with new-tracing.
     - `cardano-tracer` configured to reforward.
    
  - a sink node `$SINK_HOST` running
    - `cnsa-sink`

The configurations needed can be found in 
[the configuration directory](../config/).

Refer to [variables.sh](../scripts/variables.sh): you'll need to
`source` this on all your nodes.  Update and extend this for your
needs; you will need to change the `*_HOST` variables, but you should
be able to leave the `*_SOCK` variables alone.

This configuration will give two sampling nodes, each with 20 hot
peers, running in P2P mode.

## Extra installs

Install `autossh` on `$SINK_NODE`

    $ sudo apt install autossh

It is highly recommended to use `autossh` for port forwarding
(rather than plain `ssh`), it is a far more robust solution.

In the following we use `tmux` for convenience.  For all your nodes,

    $ sudo apt install tmux

## System Setup & Configuration

Every sampling node 
 - needs the config files
 - should have the shell variables defined 

We use `rsync` to copy the config files from your local host to the
sampling nodes:
`
    # your repo
    CNSA=~/src/iog/cardano-network-service-assurance
    source ${CNSA}/scripts/variables.sh
    ALLSAMPLINGHOSTS="${SA1_HOST} ${SA2_HOST}"
    for host in ${ALLSAMPLINGHOSTS}; do
      echo host : $host
      rsync -plv ${CNSA}/config/* $host:${NODE_CFG_RELDIR}/
      echo
    done
`

## Spinning Up
### Sampling Nodes

For each sampling node, you'll start `cardano-node` and
`cardano-tracer`, the order doesn't matter and each process is
resilient to the other process starting and stopping. Here's setting 
up `$SA1_HOST`:

    ssh $SA1_HOST
     source $CNSA/scripts/variables.sh  # or from ...

     tmux new -s cardano-tracer

      cardano-tracer --config $NODE_CFG_DIR/c-tracer-config-reforwarding.yaml

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

Note that we're using configurations configured for P2P, this is not
absolutely necessary.

Caveat: a few dependencies between variables and configuration data.
Regarding the ...
Note `config/c-tracer-config-reforwarding.yaml`

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

### Sink Node (`cnsa-sink`)

The following is executed on the `$SINK_HOST`:

    ssh $SINK_HOST
    source $CNSA/scripts/variables.sh  # or from ...

First, forward ports from all your sampling nodes to the sink host:

    tmux new -s port-forwarding
    
     [ -e $SA1_REMOTE_TRACER_SOCK ] && remove $SA1_REMOTE_TRACER_SOCK
     [ -e $SA2_REMOTE_TRACER_SOCK ] && remove $SA2_REMOTE_TRACER_SOCK

     autossh -nNT -o "ExitOnForwardFailure yes" \
       -L $SA1_REMOTE_TRACER_SOCK:$REFWD_SOCK $SA1_HOST &
     autossh -nNT -o "ExitOnForwardFailure yes" \
       -L $SA2_REMOTE_TRACER_SOCK:$REFWD_SOCK $SA2_HOST &

Note that we are connecting *not* to the `cardano-node` tracing socket, but to
the new socket reforwarded by `cardano-tracer`.

Second, start `cnsa-sink`

    tmux new -s sink
     cnsa-sink $SA1_REMOTE_TRACER_SOCK $SA2_REMOTE_TRACER_SOCK

You'll see a bit of output on `stdout`, including a lot of debugging
information.  Currently, <TODO>

## Testing the cnsa-sink service
### Storage of filtered, forwarded logs

Note that the *filtered* logs from each sampling node is being 
stored on the sink node:

    ls -rlt /tmp/cnsa-sink-m-logs/tmp-cnsa-trace-forward-sa*sock/

### Serving System Level Datapoints

We can verify that `cnsa-sink` is serving the `CNSA.BlockState` datapoint

    tmux new -s demo
     demo-acceptor Initiator \
      -s $SINKSERVER_SOCK \
      -p 2 \
      CNSA.BlockState

`demo-acceptor` polls the *new tracing protocol* on `$SINKSERVER_SOCK`
every 2 seconds, requests the `CNSA.BlockState` datapoint, and prints
the result as JSON.

CAVEAT: [the custom/updated demo-acceptor] (TODO).

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
    the time since slot start (delay) at which we become aware ... (TODO).
  - `slot_top`, of type `gauge`, is the highest slot number for a block
    that we have seen so far.
  - `slot_penultimate`, of type `gauge`, is the second highest slot number
    for a block that we have seen so far.

