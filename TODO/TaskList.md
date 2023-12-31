# Task List #
## Goals for Fall 2023 ##

  * [x] Add new CNSA analysis for *progress of syncing, e.g. fetched
        bytes over time and throughput as a function of time*
        (suggested by Marcin S on 2023-09-11).
        
  * [x] Update the documentation (particularly capturing Sam's journey
        in getting CNSA running)
        
  * [x] Update the code to address Marcin's code review (see
    2023-08-11-cardano-node-service-assurance-review.md)
    - [x] Implement the straightforward code improvements.
    - [x] Ensure any un-addressed asepects of code review are captured
          somewhere.
    
  * [x] Add InfluxDB database backend (Sam)
            
## Future Work ##

  * [ ] Add new CNSA analysis for *block content metrics*.
    - We'd like to write analyses that incorporate additional sources
      of data, e.g., information collected from transactions, the
      memory pool, blocks, and ledger tracing events.
    - What: for each block, capture the contents as seen by Ledger,
      i.e., number of TXs, number of scripts, sum of script sizes,
      maximum size(?), etc. (anything else here?)
    - What questions this could allow us to answer: We can determine
      correlations between block diffusion (already being measured)
      and block contents (TXs, scripts, etc.)
    
  * [ ] Extend cnsa-sink with configuration file for customization
        (certain data is currently hardcoded in code, all noted with
        FIXMEs).
        
  * [ ] Extend logging of cnsa-sink to handle log-rotation, etc. using
        code from cardano-tracer library.

  * [ ] Code improvement: remove dependence on locli source code,
        capture exactly what assumptions/dependences on the log files
        
  * [ ] Make trace log "dispatching" to analyses more efficient: parse
        once, dispatch once.
        
  * [ ] Improve how we add analyses
    - make them more "plugin"-like
    - more automated: then generate config files, generate documentation.

  * [ ] Extend the "Throughput" analysis to support a datapoint interface.

## Supporting the Use/Application of CNSA ##

Objective here: Make the the CNSA easier for users and the community
to learn, use, and adapt; thus extending the target set of data
consumers.

  * [ ] Create a worked use case of better integration into an
        organization’s operational management. (This will involve a bit
        of requirements gathering and such.)

The focus of CNSA has been to provide the technology to extract
real-time data, do the analysis, and "provide" (or "export") the
results of this analysis.  The focus has not been on the rendering or
"historical" renderings of data, as other tools would seem more
appropriate to this task.  However, for the sake of documentation and
instruction we *do* want to create small but varied exemplars of CNSA data
extraction and rendering.  Some potential instances of this would be

  * [ ] Provide a simple Grafana page that displays a CNSA analysis.
  * [ ] Create example threshold and alarms with Grafana.
  * [ ] Write a demo programs for cnsa-sink datapoint consumers (in Haskell).

## Future Work ##

  * [ ] Extend remote logging integration (e.g. datadog, papertrail).
