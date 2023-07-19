#!/usr/bin/env bash

# NOTE: this file not (yet) able to be run, but used for copy and pasting.

source ./variables.sh

# or, to run the above from anywhere:
source ~/src/iog/cardano-network-service-assurance/scripts/variables.sh
   # reminder: the above file up to date?


##### COPY CONFIGURATIONS TO SAMPLING NODES ###########################

# from anyplace where you have ${CNSA} repo

# ALLMYNODES=${SA1_HOST}
ALLMYNODES="${SA1_HOST} ${SA2_HOST}"
ALLMYNODES=${SINK_HOST}
ALLMYNODES=${SA1_HOST}
ALLMYNODES="${SA1_HOST} ${SA2_HOST} ${SINK_HOST} ${BUILD_HOST}"
for host in ${ALLMYNODES}; do
  echo host : $host
  rsync -plv ${CNSA}/config/* $host:${NODE_CFG_RELDIR}/
  echo
done

##### SETUP EACH SAMPLING NODE #######################################

# for each of SA1 and SA2:

ssh $SA1_HOST

 tmux new -s cardano-tracer

  # configured to be reforwarding:
  cardano-tracer --config $NODE_CFG_DIR/c-tracer-config-reforwarding.yaml
    # starting first: yes, works great.
    # Is perfectly robust wrt starting and stopping cardano-node

  # OLD, before reforwarding
    cardano-tracer --config $NODE_CFG_DIR/c-tracer-config.yaml
 
 tmux new -s cardano-node

  MYIP=$SA1_IPADDR
  MYIP=$SA2_IPADDR
  cd $NODE_CFG_DIR
   # ^ irritating: but the logfile name in config.json is 'relative'
   
  cardano-node run \
   --config $NODE_CFG_DIR/config.yaml \
   --database-path $NODE_CFG_DIR/db/ \
   --socket-path $CARDANO_NODE_SOCKET_PATH \
   --host-addr $MYIP \
   --port 1337 \
   --topology $NODE_CFG_DIR/topology.json \
   --tracer-socket-path-accept $TRACER_SOCK
      # now cardano-tracer can connect!

##### Idioms on Sampling Nodes #######################################
 
# - to re-attach
 tmux attach -t cardano-node

# - to capture panes
 tmux capture-pane -t port-forwarding -p -S - > ~/tmp/T-tmux-capture-port-C
 tmux capture-pane -t cardano-node  -p -S - > ~/tmp/T-tmux-capture-node-C
 tmux capture-pane -t aggr1         -p -S - > ~/tmp/T-tmux-capture-aggr1-C
 tmux capture-pane -t getdps1       -p -S - > ~/tmp/T-tmux-capture-getdps1-C

##### SETUP THE SINK NODE ############################################

ssh $SINK_HOST
 
 # forEach SamplingNode: forward the ports (from SA* to local sockets)
 tmux attach -t port-forwarding
 tmux new -s port-forwarding
 
  source ~/src/iog/cnsa/systems/current/scripts/variables.sh

  [ -e $SA1_REMOTE_TRACER_SOCK ] && remove $SA1_REMOTE_TRACER_SOCK
  [ -e $SA2_REMOTE_TRACER_SOCK ] && remove $SA2_REMOTE_TRACER_SOCK

  # note: connected to the reforwarded sockets (not to cardano-node's socket)
  autossh -nNT -o "ExitOnForwardFailure yes" \
    -L $SA1_REMOTE_TRACER_SOCK:$REFWD_SOCK $SA1_HOST &
  autossh -nNT -o "ExitOnForwardFailure yes" \
    -L $SA2_REMOTE_TRACER_SOCK:$REFWD_SOCK $SA2_HOST &

##### DEMO 1: NodePeers Aggregator ######################################
  
 # Aggregate all Datapoints (from FIFO) into the 'analysis'

 tmux new -s aggr1
  # or
  tmux attach -t aggr1

  source ~/src/iog/cnsa/systems/v0.5/scripts/variables.sh
  if [ ! -e $DPSFIFO1 ]; then
    echo creating $DPSFIFO1
    mkfifo $DPSFIFO1
  fi

  $EXEC_AGGREG < $DPSFIFO1
   
 # Multiplex the Datapoints from all Sampling Nodes onto a FIFO:
 tmux new -s getdps1
  # or
  tmux attach -t getdps1

  source ~/src/iog/cnsa/systems/v0.5/scripts/variables.sh
  $EXEC_GETDPS -s $SA1_REMOTE_TRACER_SOCK \
               -s $SA2_REMOTE_TRACER_SOCK \
               -p 1 NodePeers | tee $DPSFIFO1

##### DEMO 2: Aggregating BlockFetch.Client #########################
  
 # Aggregate all Datapoints (from FIFO2) into the 'analysis'

 tmux new -s aggr2
  # or
  tmux attach -t aggr2

  source ~/src/iog/cnsa/systems/v0.5/scripts/variables.sh
  if [ ! -e $DPSFIFO2 ]; then
    echo creating $DPSFIFO2
    mkfifo $DPSFIFO2
  fi

  # trivial analysis (for now)
  tail -f < $DPSFIFO2
   
 # Multiplex the Datapoints from all Sampling Nodes onto a FIFO:
 tmux attach -t getdps2
 tmux new -s getdps2

  source ~/src/iog/cnsa/systems/v0.5/scripts/variables.sh

  $EXEC_GETDPS -s $SA1_REMOTE_TRACER_SOCK \
               -s $SA2_REMOTE_TRACER_SOCK \
               -p 4 BlockFetch.Client | tee $DPSFIFO2

##### DEMO 3: Aggregating ChainSync.Client #########################
  
 # Aggregate all Datapoints (from FIFO3) into the 'analysis'

 tmux new -s aggr3
  # or
  tmux attach -t aggr3

  source ~/src/iog/cnsa/systems/v0.5/scripts/variables.sh
  if [ ! -e $DPSFIFO3 ]; then
    echo creating $DPSFIFO3
    mkfifo $DPSFIFO3
  fi

  # trivial analysis (for now)
  tail -f < $DPSFIFO3
   
 # Multiplex the Datapoints from all Sampling Nodes onto a FIFO:
 tmux attach -t getdps3
 tmux new -s getdps3

  source ~/src/iog/cnsa/systems/v0.5/scripts/variables.sh

  $EXEC_GETDPS -s $SA1_REMOTE_TRACER_SOCK \
               -s $SA2_REMOTE_TRACER_SOCK \
               -p 4 ChainSync.Client | tee ~/T-demo3-out # $DPSFIFO3


##### DEMO *: Various getdps #############################################

  $EXEC_GETDPS -s $SA1_REMOTE_TRACER_SOCK \
               -s $SA2_REMOTE_TRACER_SOCK \
               -p 30 Static.DatapointList NodeInfo \
               -p 10 BlockFetch.Client \
               | tee T-newgetdps-log
  
  
  $EXEC_GETDPS -s $TRACER_SOCK \
               -p 8 ChainSync.Client
  
##### Test reforwarding tracing ##########################################

  - $SINK_HOST  : running cardano-node and cardano-tracer (reforwarding)
  - $BUILD_HOST : running cardano-tracer that connects to ^
  
  source ~/src/iog/cnsa/systems/v0.8/scripts/variables.sh

  ssh $SINK_HOST
    tmux attach -t cardano-tracer
  
      tmp3/cardano-tracer \
          --config $NODE_CFG_DIR/c-tracer-config-reforwarding.yaml

  ssh $BUILD_HOST
    tmux attach -t cardano-tracer

      # we assume:
      #  - no cardano-node
      #  - only one cardano-tracer running on this host
      
      [ -e $TRACER_SOCK ] && remove $TRACER_SOCK
      autossh -nNT -o "ExitOnForwardFailure yes" \
              -L $TRACER_SOCK:$REFWD_SOCK \
              $SINK_HOST &

        # N.B.
        #  - don't connect to cardano-node's sock,
        #    but to the reforwarded socket!
      
      cardano-tracer --config $NODE_CFG_DIR/c-tracer-config.yaml
        # the $TRACER_SOCK is embedded in the above!    
  
      # OR

##### Test analysis-server (V0): demo-acceptor #############################

  source ~/src/iog/cnsa/systems/v0.8/scripts/variables.sh

  - setup the sink node with forwarding, as above
   
  ssh $SINK_HOST
    tmux attach -t sink

    EXEC_SINK=demo-acceptor

    $EXEC_SINK Initiator \
      -s $SA1_REMOTE_TRACER_SOCK \
      -s $SA2_REMOTE_TRACER_SOCK \
      -p 2 Bogus
    
##### Test analysis-server (V1): cardano-tracer ############################

  source ~/src/iog/cnsa/systems/v0.8/scripts/variables.sh

  - setup the sink node with forwarding, as above
   
  ssh $SINK_HOST
    tmux attach -t sink
      ~/tmp/cardano-tracer \
         --config $NODE_CFG_DIR/c-tracer-config-cnsasink.yaml
        # this config file has the two sink ports in it!
        # -- hmmm: will this ignore hasCnsaSink?
        # this should be able to co-exist with other cardano-tracers

      # if debugging, should see merged json logs on stdout.

    # verify your forwarded logs are being filtered and stored: 
    tail -f /tmp/cardano-tracer-cnsasink-m-logs/tmp-trace-forward-sa1sock/*json
    tail -f /tmp/cardano-tracer-cnsasink-m-logs/tmp-trace-forward-sa2sock/*json
    
##### Test : get DP from the analysis-server (V1) #########################

  source ~/src/iog/cnsa/systems/v0.8/scripts/variables.sh

  - start the previous
    - note: it forwards the DP on /tmp/sink-reforwarding.sock
   
  ssh $SINK_HOST

    demo-acceptor Initiator \
      -s /tmp/sink-reforwarding.sock \
      -p 2 BlockFetch.LastCompletedTimes
    
##### Test analysis-server (V2): cnsa-sink ############################

  source ~/src/iog/cnsa/systems/v0.8/scripts/variables.sh

  ssh $SINK_HOST
    tmux attach -t sink
      cnsa-sink /tmp/trace-forward-sa[12].sock

    # verify your forwarded logs are being filtered and stored:
    ll /tmp/cnsa-sink-m-logs/tmp-trace-forward-sa*
    tail -f /tmp/cnsa-sink-m-logs/tmp-trace-forward-sa1sock/*json
    tail -f /tmp/cnsa-sink-m-logs/tmp-trace-forward-sa2sock/*json

    # verify serving the DP: [my new demo-acceptor]
    demo-acceptor Initiator \
      -s /tmp/cnsa-sink.sock \
      -p 2 BlockFetch.LastCompletedTimes

    # verify serving the DP: [my new demo-acceptor, newest DP]
    demo-acceptor Initiator \
      -s /tmp/cnsa-sink.sock \
      -p 2 CNSA.BlockState
    
##### Test : Q. Are metrics being reforwarded? A. No. ######################
      
  source ~/src/iog/cnsa/systems/v0.8/scripts/variables.sh

  - setup the sink node with forwarding, as above
   
  ssh $SINK_HOST
    tmux attach -t sink
      ~/tmp/cardano-tracer \
         --config $NODE_CFG_DIR/c-tracer-config-cnsasink-prometheus.yaml
      # this config file has the two sink ports in it!
      # this should be able to co-exist with other cardano-tracers
      # we want prometheus serving here, port 4000

  #  $ curl http://localhost:4000/
  #  $ curl http://127.0.0.1:4000/tmp-trace-forward-sa1sock
  #  No metrics were received from this node.
      
  
      
