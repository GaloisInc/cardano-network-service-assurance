# Installation

See README.md for the big picture and how to spin up a CNSA instance.

## Executables

The three executables needed for a CNSA instance are the following:

 - cardano-node    (we have tested with 8.1.1)
 - cardano-tracer
 - cnsa-sink

## Building
### Preliminaries

If you have already built `cardano-node` from source, then you should
be able to build the CNSA tools.  Refer to
https://developers.cardano.org/docs/get-started/installing-cardano-node
for how to do the former.  At the moment, the above link has not been
updated to reflect a proper install of libsodium, you may need to
replace the instructions there with the following:

``` shell
git clone https://github.com/input-output-hk/libsodium
cd libsodium
git ch iquerejeta/vrf_batchverify
./autogen.sh
./configure
make
sudo make install
```

CNSA has only been tested on Linux.

### Caveats

You should *not* build `exe:cardano-node` from this repo, although it
would be allowed: the version indicates `cardano-node-8.1.1`, it is not the
same as the officially tagged `8.1.1`.  Our tools use
`lib:cardano-node`, so we must have the `cardano-node` package
available.  You are encouraged to use your standard `cardano-node`
build process, it will support "new tracing" and needs no
modifications to work as a CNSA sampling node.

You *should* build `exe:cardano-tracer` from this package. By doing
so, you will ensure you get a version, which contains the
"reforwarding tracer" extension.  (Note the comments in
`cabal.project`.)

### Building CNSA executables

If you have an environment where you have built (or can build)
cardano-node, then you should be able to build CNSA executables thus:

    cabal update
    cabal build exe:cardano-tracer
    cabal build exe:cnsa-sink

## Other Tooling 

The following is a tool ... TODO
 - `demo-acceptor`
 
