{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PackageImports #-}

module Cardano.Tracer.CNSA.Analysis.Types where

-- base:
import           Data.List (isPrefixOf)
import           Data.Set (Set)
import           Data.Text (Text)

-- package contra-tracer: (not to be confused with Cardano.Tracer....)
import qualified "contra-tracer" Control.Tracer as OrigCT

-- cardano packages:
import qualified Cardano.Logging.Types as Log
import           Cardano.Tracer.MetaTrace -- hiding (traceWith)
import           Trace.Forward.Utils.DataPoint

-- package locli: (or slice thereof)
import qualified Cardano.Unlog.LogObject as LO

-- package prometheus:
import qualified System.Metrics.Prometheus.Concurrent.Registry as PR

-- local:
import           Cardano.Utils.Log

------------------------------------------------------------------------------
-- Types

data AnalysisArgs = AnalysisArgs
  { aaRegistry :: PR.Registry             -- ^ prometheus registry
  , aaTraceDP  :: Trace IO DataPoint      -- ^ toplevel datapoint trace
  , aaDebugTr  :: OrigCT.Tracer IO String -- ^ cnsa debugging tracer
  -- TODO: see Improvement 3. below, replace stdout with this:
  -- , aaLogTr    :: OrigCT.Tracer IO String -- ^ the log for this analysis
  -- ?
  }

--------------------------------------------------------------------------------
-- Namespaces
--
-- Some of this is duplicative of code in `Cardano.Tracer.Handlers.ReForwarder`,
-- but not entirely, since that code operates on lists. We should porobably
-- upstream this version of the abstraction.

-- | The log name space, corresponds to Log.toNamespace field.
type Namespace = [Text]  --

-- | A filter for namespaces - query it with `nsFilterAllows`.
--
-- XXX: want Semigroup, Monoid, IsList, perhaps `insert`
data NamespaceFilter
  = NoFilter
  | Filter (Set Namespace)

-- | Does the `NamespaceFilter` allow the `Namespace`?
nsFilterAllows :: NamespaceFilter -> Namespace -> Bool
nsFilterAllows nsf containee =
  case nsf of
    NoFilter -> True
    Filter containers -> any (`nsContains` containee) containers

-- | Does `container` contain `containee`?
--
-- >>> ["Foo", "Bar"] `nsContains` ["Foo", "Bar", "Baz"]
-- True
--
-- >>> ["Foo", "Bar"] `nsContains` ["Foo", "Bar"]
-- True
--
-- >>> ["Foo", "Bar"] `nsContains` ["Foo"]
-- False
nsContains :: Namespace -> Namespace -> Bool
nsContains container containee = container `isPrefixOf` containee

--------------------------------------------------------------------------------

-- | Analysis - capture a 'generic' analysis that receives traceobjects
--              and updates datapoints, logs ..., and serves prometheus
--              data.
--
-- FIXME[F2]: Improvements
--  1. use/implement aTraceNames
--      - use code from updated cardano-tracer to filter and combine.
--     SamC: done, sorta, see caveat in Namespaces section above
--  2. here or _: build in code that does "overflowing" datapoints
--  3. stop writing to stdout, but ...
--    - each analysis has own file
--    - each analysis has own prefix in one logfile/logsystem?
--    - debugTracing (aaDebugTr) vs stdout vs _: get all in order
--  4. datapoint 'abstractions/improvements
--     - rather than adhoc Log.MetaTrace, can you ...
--       - enumerate datapoint types & names in Analysis?
--       - ...?
--  5. other conveniences
--    - turn [scalar] datapoints [easily/automatically] into prometheus data
--    - ?
--  6. add database hooks, e.g., a new field below:
--      aDataBaseHook       :: DBObject -> IO ()

data Analysis = forall state. Analysis
  { aName               :: String
  , aTraceNamespaces    :: NamespaceFilter
  , aInitialize         :: AnalysisArgs -> IO (Possibly state)
  , aProcessTraceObject :: AnalysisArgs
                        -> state
                        -> Log.TraceObject -> LO.LOBody -> IO ()
  }


