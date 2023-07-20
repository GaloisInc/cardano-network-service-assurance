
#### naming our nodes: sampling nodes and the sink node ####################

# two sampling nodes (capture desired IP addresses for them also):
SA1_HOST=vm-37-59.eic.galois.com
SA2_HOST=vm-36-c9.eic.galois.com
SA1_IPADDR=192.168.61.186
SA2_IPADDR=192.168.61.191

# central, sink host:
SINK_HOST=fjord.proj.galois.com

##### variables used to configure the sampling nodes #######################

# The location(s) of the Unix domain socket on each sampling HOST:
#   - N.B.: These must must match what is configured in
#   config/c-tracer-config*.yaml 

TRACER_SOCK=/tmp/cardano-node-to-cardano-tracer.sock
REFWD_SOCK=/tmp/cardano-tracer-reforwarding.sock
  
# The "home directory" for cardano-node, which holds
#  - config files, database, ports, etc.
#    (even logs did we not change the configuration)

NODE_CFG_RELDIR=iog-cardano-mainnet
NODE_CFG_DIR=$HOME/$NODE_CFG_RELDIR

if [[ -e $NODE_CFG_DIR ]]; then
  export CARDANO_NODE_SOCKET_PATH="$NODE_CFG_DIR/db/node.socket"
fi  

##### variables used to configure the $SINK_HOST ###########################

SA1_REMOTE_TRACER_SOCK=/tmp/trace-forward-sa1.sock
SA2_REMOTE_TRACER_SOCK=/tmp/trace-forward-sa2.sock

# The socket where cnsa-sink serves the tracing protocol, currently
# only datapoints are being served here:

SINKSERVER_SOCK=/tmp/cnsa-sink.sock
  # don't change this as it is [currently] wired into cnsa-sink.
