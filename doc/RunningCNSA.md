# Spinning Up and Using CNSA

## Extra installs

The below scripts assume you have installed:

autossh 
tmux
demo-acceptor 

It is highly recommended to use `autossh` for port forwarding
(vs. plain `ssh`), it is a far more robust solution.

## TODO ...

- starting cardano-node vs. cardano-tracer: order doesn't matter, and
  each is resilient to the other starting and stopping.
  
- using P2P [not necessary]

## Note `config/c-tracer-config-reforwarding.yaml`

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

## Usage / cnsa-sink

 http://localhost:8080/metrics

