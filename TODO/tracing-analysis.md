# Tracing in Cardano: Analysis, Recommendations #
## Audience ##

Apologies to engineers already familiar with the Cardano logging
system: some of the below will be familiar or redundant; the goal was
to make this somewhat accessible to others.

## Summary ##

We provide an assessment of the tracing system of cardano-node and components.
In particular, we assess it
 - not just as a stand-alone system to go from traces in the application
   code to log files (as one would use on a cardano node), but also
 - from the viewpoint of other and higher level consumers of the trace
   data such as the CNSA system, network health tools, `pooltoolio`, etc.

## Background ##
### The Cardano Tracing System ###

We assume some familiarity with the Cardano tracing (logging) system as well as
the "new tracing" system (based on trace-dispatcher and
cardano-tracer). See in particular, [New Tracing
Quickstart](https://github.com/input-output-hk/cardano-node-wiki/wiki/New-Tracing-Quickstart)
which refers to these two documents [Cardano
Tracer](https://github.com/input-output-hk/cardano-node/blob/master/cardano-tracer/docs/cardano-tracer.md)
and [trace-dispatcher: efficient, simple and flexible program
tracing](https://github.com/input-output-hk/cardano-node/blob/master/trace-dispatcher/doc/trace-dispatcher.md).

The design is based on contravariant tracing, for a birds-eye
perspective of contravariant tracing and its advantages, see Duncan
Coutts' MuniHac 2020 talk:

  [Contravariant logging: How to add logging without getting grumpy](https://industry.haskell.org/blog/2020/09/munihac-2020/)

In new-tracing, `cardano-node`, the producer of traces, communicates
with `cardano-tracer`, a consumer of traces, using a socket running
the "new-tracing protocol (NT-PRTCL)". See
[here](https://github.com/input-output-hk/cardano-node/blob/master/cardano-tracer/docs/cardano-tracer.md#overview)

### Three Main Components of the Cardano Tracing System ###

At a very high level, we can view the Cardano tracing system as having three
separate components:
  1. The tracers that have been added to the application code: i.e., trace
     producers.
  2. The consumers of the traces such as the logging system, datapoints, EKG, etc.
  3. The *tracing infrastructure* which brings 1. and 2. together:
     this isn't trivial, a large part of this is the code that threads
     ``top level'' tracers down to **each** of the components of the
     application code.

### Some Terminology ###

Following in the spirit of the above references, we are intentionally
using "tracing" as a more general term than "logging": logging is
ultimately just about creating and storing log messages, while tracing could be
used to monitor or analyze programs in the general sense; tracing
could involve gathering metrics, EKG data, benchmarking, testing,
etc.

At the code level, we refer to the callers of `traceWith` as
"application code" or as the trace-emitting program
([see](https://github.com/input-output-hk/cardano-node/blob/master/trace-dispatcher/doc/trace-dispatcher.md#overview-and-terminology))

**Trace Consumers.**
Above we referred to `cardano-tracer` as a consumer of traces, but
there are a growing number of tools that are the ultimate consumers
of the traces, these consume traces at a higher or semantic level:
  - programs that filter and/or parse the log files, e.g.,
    - CNSA
    - the `locli` benchmarking tools
    - various versions of community tools (cncli, sendmytip.sh, ...)
  - any program that uses the new Datapoint feature
  - clients to the EKG and Prometheus services of `cardano-node` (or,
    with new-tracing, as served by `cardano-tracer`)

Other terms and acronyms
 - NT-PRTCL : the New Tracing Protocol running between `cardano-node` and `cardano-tracer`.
 - CNSA : the Cardano Network Service Assurance system.
 - App-Tracing-Types : the types used by the application code which
   are the arguments to our contravariant tracers.

## Assessments ##
### Summary ###

The current design and implementation of the tracing system is
comprehensive, elegant, and powerful.  By using contravariant
tracing, the system is able to be
- modular: application code need only know the *App-Tracing-Types*,
   application code need not embed kitchen-sink monads,
   application code needs to know nothing of log formatting, log
   identifiers, severity levels, etc.
- efficient: unused traces need not cost more than an "empty function
  call"
- configurable: code outside application code can adjust the logging
  with tuning, enabling/disabling, etc.
- general: using tracing combinators: traces can be turned into
  metrics, stateful metrics, datapoints, as well as logging events.

With new-tracing, the tracing "consumers" need not even be in the same process.

However, the tracing system at the moment is
 - focused on tracing & logging, and
 - not yet focused on
   - the future potential when new-tracing is adopted, in particular
     possible performance gains.
   - the growing ecosystem of code, tools, systems that require
     reliable and robust access to logging data, either "real-time"
     or offline; CNSA being one of these.

### Issues Detailed ###

As a result of our engagement with all three components of the tracing
system and using it in the *new-tracing* mode, we have noticed a few
"issues," areas for improvement, and pain points:

- Implementation and efficiency issues:
  - when using the *NT-PRTCL* to do "distributed" logging and analysis:
    - the to/from JSON of datapoints and logs seems computationally inefficient.
    - bandwidth issues
      - JSON takes more space than CBOR (or the like)
      - the protocol is sending *both* human and machine
        oriented already rendered logging messages: it seems that the code that turns
        the types into human/machine should be on the *tracing
        consumer* side of this protocol.

- Maintenance and automation
  - when adding new tracers to application code---or when adding
    datapoint/tracer abstractions---it *was* some effort to discover
    where and how we needed to update the *tracing infrastructure*
    component.  I.e., we must update
    `cardano-node/src/Cardano/Node/Tracing/Tracers.hs` or modules it uses.

  - When adding/extending *App-Tracing-Types*, this involved a bit of
    code searching to find the right module to add to/from JSON instances.
    - Types involving associated types, and other ``fancy-types'',
      made writing (or deriving) To/FromJSON instances challenging.
    - In the final analysis, there was uncertainty:
      - Were the To/FromJSON instances correct? For all protocol-versions?
      - What other protocol-versions would the new tracer code support?

- Robustness and clarity issues:
  - a large number of by hand, and sometimes ad hoc, ToJSON/FromJSON instances:
    - this code is tedious, thus prone to error, and induces very
      large multi-module/multi-repository dependencies.
    - as a result correctness is *unlikely* anytime we parse JSON.
      - i.e., any client of the *NT-PRTCL* protocol must parse log messages

- Versioning and known version compatibility:
  - Currently *any* change to tracing *App-Tracing-Types* or *ToJSON*
    instances or various logging code is a potential breaking change
    for the growing list of 3rd party log file consumers.
  - If there are conventions, or best-practices, or mechanisms to
    allow for compatibility of log messages, these never became
    apparent to us.
  - We have noticed code in "cardano-node" (`bench/locli/src/Cardano/Unlog/LogObject.hs`)
    that attempts to parse log files for legacy systems. (This is code we
    have copied for CNSA.)  A more general solution seems worthwhile!

The consequence of many of the above is more work and less than ideal solutions for
tracing consumers such as
  - ad hoc log parsing in CNSA
  - ad hoc log parsing in the tracing/benchmarking code (in `cardano-node/bench/*`)
  - third party code that parses log files
    - NOTE: any log printing change is a potentially breaking change
      for the consumers.

### And Further Details With Regard to ToJSON/FromJSON ###

The following is taken from a previous slack message on this subject:

Obstacles & Issues
  1. The ToJSON instances are generally written by hand.
  2. The ToJSON "coverage" is patchy at the moment, sometimes only the
     encapsulating type has an instance, sometimes only the "larger
     class" ToObject has an instance.
  3. For many "tracing types", the ToJSON/ToObject instance only
     encodes parts of the type (often fields are omitted, constructors
     too?).
  4. FromJSON is lacking, or is by hand, or is being duplicated: very
     patchy in the cardano-node code.  done systematically in locli
     (but by hand) the moral equivalent is done by SPO tools that
     parse the JSON formatted logs.
 5. JSON has issues:
    - inefficient to encode/decode
    - is less compact than other methods
    - does not allow for partial parsing or bespoke encodings
 6. We have pervasive, overloaded code using this "overload over block" design pattern

      ``` haskell
      f :: (TypeClass1 block, ...) => AssocType1 block -> ... -> G (AssocType2 block, ...)

      ```

    And the tracing data types, in general, are parameterized
    over `block` (or indirectly through an associated type). Thus, we
    have a bit more complexity.
     * NOTE: I assume this is primarily in support of the hard-fork combinator.
     * NOTE: Logging code is serializing data from such overloaded
       functions on block.  Clearly this works because at this level,
       there's only one "top level" type instantiation of `block` for
       the top level functions (e.g., the logging stuff).  But wait,
       Is that really true?  As we bootstrap and get sync-ed up, are
       we logging per the era/protocols that the old blocks were in?
       And will the same thing happen when a hard-fork is occurring?
       One way or another, we do need to take care when decoding:
       we'll need a concrete type to decode to (this will not be
       overloaded over blocks).

## Brainstorming for an Ideal Tracing System ##

We list here features we might like in a future (idealized) tracing
system; some of these features currently exist to lesser or greater
degrees in the system. (We leave out implementation issues.)  Some of
these are speculative; not all may be necessary. Here's the list:

- Documentation and usability
  - We want to be able to enumerate all *traces*
    - both at run-time and statically
    - log messages as well as EKG-ish and tracing/metrics.
    - with the ability to extract for each
      - hierarchical-names
      - types
      - documentation
  - this should be correct by design, see *Maintainability and automation* below.

- Granularity
  - We want a hierarchical name space for every trace.
    - Q. Is this the same as the name space for logging messages?
  - We want to enable/disable any trace in this name space.
  - Add further granularity: treat *App-Tracing-Type* constructors and
    fields in those constructors as further sub-names in the hierarchical name
    space; we should be able to enable/disable these.
    - ideally this should be efficient: disabled constructors and
      fields should not be evaluated by the application code.
  - NOTE
    - This seems to somewhat overlap the verbosity, privacy, and
      severity aspects of the logging system. This seems to be a
      typical redundancy in logging systems.  Is there a better way?
      Move verbosity/privacy/severity aspects into ...??  Have
      abstractions over the enabling/disabling of the trace hierarchy?

- Versioning, backward compatibility, future-proofing, etc.:
  - We want to distinguish traces that are
    1. external and supported (advertised, not changed without warning)
    2. internal or unsupported (e.g., the application programmer can add/change
       to his heart's content knowing he will break no other code)
  - We need (typed) tracing that is to known to support a specific node version or eras.
    - without unnecessarily having incompatibilities between versions.
    - allowing for abstractions over traces that allow backward-compatibility
    - the correspondence between these two should be clear and explicit:
      1. the hierarchy of (external) typed tracers,
      2. the cardano-node versions (or protocol version or ...) supported.
  - We'd like to make the external/internal distinction also for constructors and
    fields (see *Granularity* above).
  - While adding granularity and external/internal support, we want to
    - keep details and complexity out of the application code;
    - however, the definition of the *App-Tracing-Type* could be exactly
      where we want to encode such information.
  - How this would work in practice:
    1. An application programmer adds a constructor/field to their *App-Tracing-Type*.
    2. Programmer notates this as `internal` (in some manner)
    3. No other code needs to change for the programmer to privately
       consume that tracing data.
       - e.g., no ad hoc ToJSON/FromJSON instances!
       - NOTE: no trace consumers should break as a result of this!

- Maintainability and automation:
  - There must be a "single source of truth" regarding current traces,
    their types and their hierarchical names from which
    - we can generate efficient code to serialize/unserialize trace data
      as CBOR (or the like).
    - we generate code to read/write traces as JSON values
      - we generate JSON schemas for each type
      - should be correct by design
      - or is there some JSON based library we'd provide. ?
    - we generate an API for accessing values (?)
      - A library that's part of cardano-node?
      - Haskell based?
      - C based??
    - we generate list of datapoints, log messages & types, &
      documentation (see *documentation and usability* above).

- Various design principles
  - when we switch to new tracing and use *NT-PRTCL*: there should be
    no lost capabilities, and minimal performance cost.
  - tracing itself should be *pay as you go* as much as possible:
    - few/zero computation costs for *any* traces we have disabled
      statically.
    - few/zero computation costs for traces we disable dynamically.
      (?)

## Avenues for Improvement ##

The previous section is about features and requirements, not about
mechanisms or design choices.   First observations
  - More exploration as to solutions is needed.
  - Socialization of possible or incremental solutions will be needed;
    clearly the logging system affects most of the Engineering teams!

We know that the benchmarking/tracing team has thought about many of
these issues and the other Engineering teams have been dealing with
the same pain points that we have in developing CNSA.

Some first ideas (low lying fruit?):

  - With new-tracing and the *NT-PRTCL*: we are sending post-rendered
    logs between processes (in CNSA these logs are transferred
    between hosts): this seems to be not only inefficient, but it involves
    breaking the *abstraction* of an `App-Tracing-Type` at the wrong
    place, can we keep this type unrendered for longer, until demanded?

  - Might we easily switch to CBOR for both
    - the datapoint representation
    - the on-the-wire representation for *NT-PRTCL*.

  - It appears that using cborg `CBOR` (or the serialize class
    `Serialize`) could
    - make it easier to generate generic definitions of
      encode/decode,
    - give us more assurance of the encode/decode "round trip"
      property,
    - and even allow for the reuse of the `App-Tracing-Type` in the
      trace consumer.  This has not proven feasible with the current
      approach.

Other thoughts

  - Other systems have solved the protocol-subset/superset versioning
    problem (e.g., the venerable ASN.1).

  - ASN.1 might also serve as inspiration as it deals properly with
    the whole issue of *hiding* the encodings of *abstract data*.

A paradigm shift?
  - The **old** paradigm: contravariant tracing provides an elegant
    foundation for logging: thus, we immediately serialize and
    we're now back to the status quo; but we haven't used our
    contravariant tracers to their potential.

  - The **new** paradigm: contravariant tracing provides new
    capabilities and power: we can
    - Do distributed *tracing*: a producer can be in a different
      process than a consumer, the consumer shouldn't have to know the
      difference.
    - Defer trace/log *rendering* to later, further-away, or never.
    - Create more lazy/demand-based solutions than currently.

  - In the *new* paradigm: our three components (see *Three Main
    Components...* above) would be
    1. trace producers: unchanged.
    2. trace consumers: how about turning this into an API?
      - not necessarily *this* anymore: write a config file and parse a
        log file.
      - but we might have this: the trace consumer is code that uses
        some API: this API can demand values, control precision?, etc.
        Ideally this code could exist in a separate process than the
        trace producer is in.
    3. tracing infrastructure: this may need to be "re-imagined",
       but the function here is simply to efficiently connect the trace
       consumers to the trace producers.

  - In the new paradigm we *hide* the representation of traces: this
    provides expressiveness.  It seems that, with the old paradigm, we
    are likely to fall into the trap of
    - using the trace *rendering* as the trace *representation*.
