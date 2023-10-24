# Task List #
## Goals for Fall 2023 ##

  * [ ] Add new CNSA analysis for *progress of syncing, e.g. fetched
        bytes over time and bandwidth as a function of time*
        (suggested by Marcin S on 2023-09-11).
        
  * [ ] Update the documentation (particularly capturing Sam's journey
        in getting CNSA running)
        
  * [ ] Update the code to address Marcin's code review (see
    2023-08-11-cardano-node-service-assurance-review.md)
    - [x] Implement the straightforward code improvements.
    - [ ] Ensure any un-addressed asepects of code review are captured
          somewhere.
    
  * [ ] Add InfluxDB database backend (Sam)
            
## Priority Two Goals ##

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
