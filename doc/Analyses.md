# Cardano Network Service Assurance (CNSA): Analyses
## The General, and Future, Approach to Extending CNSA

Here we delve deeper into the CNSA implementation than is done in
[Design and Architecture](DesignAndArchitecture.md).

CNSA currently is composed of *three* `Analyses`.  Each analysis
  - has state,
  - receives trace objects from all the sampling nodes,
  - can send results to any combination of the following:
    - `stdout`
    - the debugging logger (currently on `stdout`)
    - InfluxDB
    - the DataPoint server (each analysis can add any number of
      DataPoints to the *one* DataPoint server).
    - the Prometheus server (each analysis can add any number or types
      of Prometheus metrics to the *one* Prometheus server).

Note that currently the various analyses must ensure that their
"services" don't overlap, i.e.,
  - outputs to InfluxDB don't conflict;
  - the datapoints and the metrics don't have the same names.

## As instantiated in source code

If we dive into `src/Cardano/Tracer/CNSA/` we find this structure:

     CNSA
     ├── Analysis
     │   ├── Catalog
     │   │   ├── BlockState
     │   │   │   ├── Analysis.hs
     │   │   │   ├── DB.hs
     │   │   │   └── Types.hs
     │   │   ├── CountTraceLogs
     │   │   │   └── Analysis.hs
     │   │   └── Throughput
     │   │       └── Analysis.hs
     │   ├── Catalog.hs
     │   └── Types.hs
     └── Run
         ├── CnsaSink.hs
         ├── ParseLogs.hs
         └── PlumbAnalyses.hs

The directories under `CNSA/Analysis/Catalog/` hold the current three
analyses.  `CNSA/Analysis/Catalog.hs` is where we enumerate them:

    analyses :: [Analysis]
    analyses =
      [ Cardano.Tracer.CNSA.Analysis.Catalog.BlockState.Analysis.analysis
      , Cardano.Tracer.CNSA.Analysis.Catalog.CountTraceLogs.Analysis.analysis
      , Cardano.Tracer.CNSA.Analysis.Catalog.Throughput.Analysis.analysis
      ]

`Analysis` is an existential type that captures the essence of an
analysis; after initialization, an analysis is passed trace objects
via `aProcessTraceObject`.

    data Analysis = forall state. Analysis
      { aName               :: String
      , aTraceNamespaces    :: NamespaceFilter
      , aInitialize         :: AnalysisArgs -> IO (Possibly state)
      , aProcessTraceObject :: AnalysisArgs
                            -> state
                            -> Log.TraceObject -> LO.LOBody -> IO ()
      }

## Current Analyses

The current analyses are
1. **BlockState**:  For each block and for each sampler node keep
   track of the propagation delay for each `(block,sampler,peer)` as
   well as other timing information. This serves

   - The `CNSA.BlockState` datapoint, and
   - the following Prometheus metrics

          # TYPE propDelays histogram
          propDelays_bucket{le="0.1"} 822.0
          propDelays_bucket{le="0.2"} 30709.0
          ...
          propDelays_sum  484713.4123378238
          propDelays_count  578843

          # TYPE slot_penultimate gauge
          slot_penultimate  9.833154e7

          # TYPE slot_top gauge
          slot_top  9.8331553e7


2. **CountTraceLogs** : Count the the number of trace events received
   by `cnsa-sink`: serves both as a Hello World and as a sort of EKG.
   Serves this prometheus metric:

        # TYPE count_of_tracelogs counter
        count_of_tracelogs  21045


3. **Throughput**: for each sampling node, keep the running total of
   all block sizes downloaded. Serves this labeled prometheus metric:

        # TYPE blockfetching_total_bytes counter
        blockfetching_total_bytes{host="mysamplehost1"} 26842211
        blockfetching_total_bytes{host="mysamplehost2"} 30867118
        ...

    From this, one may easily view or compute the throughput rates of
    sampler nodes, e.g., using the Prometheus query,

        rate(blockfetching_total_bytes[5s])

## Future Work

- Generate documentation for the analyses automatically.
- Further improve the the code to allow for more of a "plug and play"
  analysis: e.g., add, remove, link new analysis, etc.; turning
  on/off, etc.
- Improve the efficiency in dispatching trace events to analyses.
- Remove possibility of analyses "accidentally" overlapping in their
  metrics, datapoints, or InfluxDb database writes.
