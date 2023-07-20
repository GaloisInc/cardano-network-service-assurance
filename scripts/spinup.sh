#!/usr/bin/env bash

# NOTE: this file not (yet) able to be run, but used for copy and pasting.

##### Local ###########################################################

# set this to this repo location:
CNSA=~/src/iog/cardano-network-service-assurance

BUILD_HOST=sapphire.proj.galois.com

# set the variables:
source ${CNSA}/scripts/variables.sh

##### Local: idioms on Sampling Nodes #################################
 
# - to re-attach
 tmux attach -t cardano-node
# - to capture panes
 tmux capture-pane -t cardano-node  -p -S - > ~/tmp/T-tmux-capture-node-C


##### COPY CONFIGURATIONS TO SAMPLING NODES ###########################

# from anyplace where you have ${CNSA} repo

ALLHOSTS=${SA1_HOST}
ALLHOSTS="${SA1_HOST} ${SA2_HOST}"
ALLHOSTS="${SA1_HOST} ${SA2_HOST} ${SINK_HOST} ${BUILD_HOST}"

ALLSAMPLINGHOSTS="${SA1_HOST} ${SA2_HOST}"
for host in ${ALLSAMPLINGHOSTS}; do
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

  # Did you want to just use new-tracing and not do CNSA:
    cardano-tracer --config $NODE_CFG_DIR/c-tracer-config.yaml
 
 tmux new -s cardano-node

  MYIP=$SA1_IPADDR  #
  cd $NODE_CFG_DIR
    # (this a little irritating: but the logfile name in config.json is 'relative'
    # HUH?
    # ALSO: p2p!
  
  cardano-node run \
   --config $NODE_CFG_DIR/config-p2p.yaml \
   --database-path $NODE_CFG_DIR/db/ \
   --socket-path $CARDANO_NODE_SOCKET_PATH \
   --host-addr $MYIP \
   --port 1337 \
   --topology $NODE_CFG_DIR/topology.json \
   --tracer-socket-path-accept $TRACER_SOCK
      # now cardano-tracer can connect!

##### SETUP THE SINK NODE ############################################

ssh $SINK_HOST
 
 # forEach SamplingNode: forward the ports (from SA* to local sockets)
 tmux new -s port-forwarding
 
  source $CNSA/scripts/variables.sh

  [ -e $SA1_REMOTE_TRACER_SOCK ] && remove $SA1_REMOTE_TRACER_SOCK
  [ -e $SA2_REMOTE_TRACER_SOCK ] && remove $SA2_REMOTE_TRACER_SOCK

  # note: connect to the cardano-tracer's reforwarded socket (not to
  # cardano-node's socket)
  autossh -nNT -o "ExitOnForwardFailure yes" \
    -L $SA1_REMOTE_TRACER_SOCK:$REFWD_SOCK $SA1_HOST &
  autossh -nNT -o "ExitOnForwardFailure yes" \
    -L $SA2_REMOTE_TRACER_SOCK:$REFWD_SOCK $SA2_HOST &

  
##### Test cnsa-sink ################################################

ssh $SINK_HOST

 tmux attach -t sink

  source $CNSA/scripts/variables.sh
  cnsa-sink $SA1_REMOTE_TRACER_SOCK $SA2_REMOTE_TRACER_SOCK

  # verify your reforwarded logs are being filtered and stored:
  tail -f /tmp/cnsa-sink-m-logs/tmp-trace-forward-sa1sock/*json
  tail -f /tmp/cnsa-sink-m-logs/tmp-trace-forward-sa2sock/*json

  # verify cnsa-sink is serving the Datapoint:
  #  CAVEAT: [updated demo-acceptor, newest Datapoint]
  demo-acceptor Initiator \
    -s $SINKSERVER_SOCK \
    -p 2 CNSA.BlockState
    
# TODO: rename demo-acceptor to _! (or change to old syntax)

 
  
      
