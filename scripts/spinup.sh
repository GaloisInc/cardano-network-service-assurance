#!/usr/bin/env bash

# NOTE: this file not (yet) able to be run, but used for copy and pasting.

##### Local ###########################################################

# set this to this repo location (parent directory)
CNSA=~/src/iog/cardano-network-service-assurance

BUILD_HOST=sapphire.proj.galois.com

# set the CNSA variables:
source ${CNSA}/scripts/variables.sh

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
 source $CNSA/scripts/variables.sh  # or from ...

 tmux new -s cardano-tracer

  cardano-tracer --config $NODE_CFG_DIR/c-tracer-config-reforwarding.yaml

  # Did you want to just use new-tracing and not do CNSA:
    cardano-tracer --config $NODE_CFG_DIR/c-tracer-config.yaml
 
 tmux new -s cardano-node

  MYIP=$SA1_IPADDR  # adjusting for each sampling node
  
  cardano-node run \
   --config $NODE_CFG_DIR/config-p2p.yaml \
   --database-path $NODE_CFG_DIR/db/ \
   --socket-path $CARDANO_NODE_SOCKET_PATH \
   --host-addr $MYIP \
   --port 1337 \
   --topology $NODE_CFG_DIR/topology-p2p.json \
   --tracer-socket-path-accept $TRACER_SOCK

      # now cardano-tracer can connect!

##### SETUP THE SINK NODE ############################################

ssh $SINK_HOST
 source $CNSA/scripts/variables.sh  # or from ...
 
 # forEach SamplingNode: forward the ports (from SA* to local sockets)
 tmux new -s port-forwarding
 
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
 source $CNSA/scripts/variables.sh  # or from ...

 tmux new -s sink

  cnsa-sink $SA1_REMOTE_TRACER_SOCK $SA2_REMOTE_TRACER_SOCK

  # verify your reforwarded logs here have been filtered:
  tail -f /tmp/cnsa-sink-m-logs/tmp-cnsa-trace-forward-sa1sock/*.json
  tail -f /tmp/cnsa-sink-m-logs/tmp-cnsa-trace-forward-sa2sock/*.json

 tmux new -s demo
  
  #  CAVEAT: [updated demo-acceptor, newest Datapoint]
    demo-acceptor Initiator \
      -s $SINKSERVER_SOCK \
      -p 2 CNSA.BlockState
    
# TODO: rename demo-acceptor to _! (or change to old syntax)

 
  
      
