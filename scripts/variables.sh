
#### variables for scripts #################################################

# two sampling nodes
SA1_HOST=vm-37-59.eic.galois.com
SA2_HOST=vm-36-c9.eic.galois.com
# central node
SINK_HOST=fjord.proj.galois.com
BUILD_HOST=fjord.proj.galois.com
BUILD_HOST=sapphire.proj.galois.com

# these might change
SA1_IPADDR=192.168.61.186
SA2_IPADDR=192.168.61.191
SINK_IPADDR=192.168.55.28

# the location(s) of the Unix domain socket on each sampling HOST:
# - UGH: these are also embedded in systems/v0.8/config/c-tracer-config*.yaml 
TRACER_SOCK=/tmp/forwarder.sock
REFWD_SOCK=/tmp/reforwarder.sock
  
  # TODO: improve names: /tmp/cardano-node-to-cardano-tracer.sock
  # TODO: improve names: /tmp/cardano-tracer-reforwarding.sock

# locations of the Unix domain sockets on the SINK ??

REMOTE_REFWD_SOCK=/tmp/reforwarder-from-remote.sock
SA1_REMOTE_TRACER_SOCK=/tmp/trace-forward-sa1.sock
SA2_REMOTE_TRACER_SOCK=/tmp/trace-forward-sa2.sock

  # In theory, these could be host:port, BUT the current $EXEC_GETDPS
  #   assumes path to a local socket.

# regarding 'home' for cardano-node (generic per sampling node):
#  - home for database, ports, etc (logs if you don't change config)

NODE_CFG_RELDIR=iog-cardano-mainnet
NODE_CFG_DIR=$HOME/$NODE_CFG_RELDIR
if [[ -e $NODE_CFG_DIR ]]; then
  export CARDANO_NODE_SOCKET_PATH="$NODE_CFG_DIR/db/node.socket"
fi  
  #  - not having to with tracing.
  #  - used by cardano-ping for local communication
  #  - other tools too?

# your repo location

CNSA=~/src/iog/cnsa  

#### More details of the system v-0.5 implementation #########################

# These just make sense on the SINK_HOST (build host too)

# FIFO's for multiplexing DataPoints onto
DPSFIFO=/tmp/dps-fifo
DPSFIFO1=/tmp/dps-fifo1
DPSFIFO2=/tmp/dps-fifo2
DPSFIFO3=/tmp/dps-fifo3

#### ... #####################################################################

# Executable to gather DataPoints
EXEC_GETDPS=demo-acceptor

# not about datapoints anymore:
RUNSINK=demo-acceptor

# Executable that aggregates all DataPoints (and does analysis)
EXEC_AGGREG=demo-node-aggregator
