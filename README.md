# Tools for Cardano Network Service Assurance (CNSA)

This package provides a set of tools that allow for doing network
service assurance of the cardano network using a set of "sampling"
Cardano nodes.

## High Level Architecture

The high level architecture of a CNSA instance comprises the following:
- Deploy N "sampling" nodes that sample data from the network (N ≈ 10 ?)
- Each node would be connected to V peers (V ≈ 100 ?)
- Sampling nodes process and send a subset of the data to a
  centralized service (`cnsa-sink` running on the "sink node").
- Centralized service---in real time---aggregates data, runs analyses
  on them, and serves the results via
  - Logs
  - DataPoints (allows for Haskell programs to read arbitrary
    datatypes over socket)
  - Prometheus  (for “scalar” network metrics)

Refer to [Architecture: CNSA Deployed](doc/architecture-deployed.svg).

CNSA achieves many of the same objectives as pooltool.io, for instance.

## Design of Sink and Sampling Hosts


Refer to [Architecture: Sink Host, Sampling Host](doc/architecture-hosts.svg)
for further details of the sink host and the sampling hosts. 
Note the following

- A.
- B.

## Documentation

- See the [Installation Instructions](INSTALL.md) for how to build the
  needed executables.

- See the [Running CNSA](doc/RunningCNSA.md) for how to spin up an
  instantiation of CNSA inside the Cardano network.

- Other documentation can be found in the [document directory](doc/).

