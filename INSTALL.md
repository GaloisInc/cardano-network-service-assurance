# Installation

## Executables

The three executables needed for a CNSA instance are the following:

 - `cardano-node`    (we have tested with 8.1.1)
 - `cardano-tracer`
 - `cnsa-sink`

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

### Caveats & Recommendations

Our tools use `lib:cardano-node`, so we must have the `cardano-node`
package available. However, ...

It is not recommended to build `exe:cardano-node` from this repo, you
are encouraged to use your standard `cardano-node` build process, which
will support "new tracing" and needs no modifications to work as a
CNSA sampling node.

However, one *could* build `exe:cardano-node` from this repo.  The
version, in cabal.project, is currently `cardano-node-8.1.1`, not the
same as the officially tagged `8.1.1`.

You *must* build `exe:cardano-tracer` with support for "re-forwarding
tracer" extension.  You can do this by either
 - building it in this package, note the comments in `cabal.project`!
 - building it in the `cardano-node` repo with an unreleased version:
   - i.e., `master` and tag `8.2.1-pre` do support re-forwarding
   - tag `8.1.2` does not support re-forwarding.

### Building CNSA executables

If you have an environment where you have built (or can build)
`cardano-node`, then you should be able to build CNSA executables thus:

    cabal update
    cabal build exe:cardano-tracer
    cabal build exe:cnsa-sink
    cabal build exe:demo-acceptor

This last is not essential, but we use it to test that we can access Datapoints.
