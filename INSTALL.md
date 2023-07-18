# The Big Picture

See README.md for the big picture and how to spin up a CNSA instance.k

# Executables

The three executables needed for a CNSA instance are the following:

 - cardano-node    (we have tested with 8.1.1)
 - cardano-tracer
 - cnsa-sink
 
The following is a tool ... <TODO>
 - dp-get
 
# Building
## Preliminaries

## Warnings

You should *not* build `exe:cardano-node` from this repo, although it
is allowed: the version indicates `cardano-node-8.1.1`, it is not the
same as the officially tagged `8.1.1`.  Our tools use
`lib:cardano-node`, so we must have the `cardano-node` package
available.  You are encouraged to use your standard `cardano-node`
build process, it will support "new tracing" and needs no
modifications to work as a CNSA sampling node.

You *should* build `exe:cardano-tracer` from this package. By doing
so, you will ensure you get the "right" version, which contains the
newest "reforwarding tracer" code.

## Building executables

Building the executables is easy

    cabal update
    cabal build exe:cnsa-sink
    cabal build exe:cardano-tracer
