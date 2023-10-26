{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use forM_" #-}

module Cardano.Tracer.CNSA.BlockState
  ( BlockData (..),
    BlockProps (..),
    BlockTiming (..),
    Sampler,
    BlockState,
    BlockStateHdl,
    updateBlockProps,
    sortBySlot,
    addSeenHeader,
    addFetchRequest,
    addFetchCompleted,
    addAddedToCurrentChain,
    modifyBlockStateIOMaybe,
    newBlockStateHdl,
    readBlockStateHdl,
    updateBlockState,
    updateBlockStateByKey,
    updateBlockStateMaybe',
    pruneOverflow
  )
where

{-
Secrets kept:
- `BlockState` is a `Map`
- `BlockStateHdl` is an `IORef`
-}

import           Cardano.Analysis.API.Ground (Hash)
import           Cardano.Slotting.Block (BlockNo)
import           Cardano.Slotting.Slot (SlotNo)
import           Cardano.Tracer.CNSA.ParseLogs (Peer,Sampler)
import           Cardano.Utils.Log (warnMsg,Possibly)
import           Data.Aeson (ToJSON, FromJSON)
import           Data.IORef ( IORef,
                   atomicModifyIORef',
                   modifyIORef',
                   newIORef,
                   readIORef,
                   writeIORef)
import           Data.List (sortOn)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map,(!?))
import           Data.Maybe (fromJust)
import           Data.Maybe.Strict (StrictMaybe, strictMaybeToMaybe)
import           Data.Ord (Down (..))
import           Data.Time (UTCTime)
import           GHC.Generics (Generic)


--------------------------------------------------------------------------------
-- BlockData

data BlockData = BlockData
  { bd_props  :: BlockProps,              -- ^ constants for a given block.
    bd_timing :: Map Sampler BlockTiming  -- ^ timing info for each sampling
                                          --   invariant: Map not empty.
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | data intrinsic to a block.
data BlockProps = BlockProps
  { bp_blockNo :: BlockNo,
    bp_slot    :: SlotNo,
    bp_size    :: Maybe Int
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | block timing data with respect to a sampling node [and its peers]:
data BlockTiming = BlockTiming
  { bt_downloadedHeader    :: Map Peer UTCTime,
    bt_sendFetchRequest    :: Map Peer UTCTime,
    bt_completedBlockFetch :: Map Peer UTCTime,
      -- KK: CompletedBlockFetch*, this trace is in the wrong
      -- place. We need a trace for when the block has been
      -- downloaded, see #4226.
    bt_addedToCurrentChain :: Maybe UTCTime
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

updateBlockTimingForSampler
  :: Sampler
  -> (BlockTiming -> BlockTiming)
  -> BlockData -> BlockData
updateBlockTimingForSampler shost update bd = bd{bd_timing= timing}
  where
  timing = Map.alter alterBlockTiming shost (bd_timing bd)

  alterBlockTiming =
    \case
      Nothing -> Nothing
      Just bt -> Just (update bt)
  -- FIXME[R3]: extend to Possibly so callers can do likewise
  -- FIXME: if shost not in Map, need to add & initialize.

updateBlockProps :: (BlockProps -> BlockProps) -> BlockData -> BlockData
updateBlockProps upd b = b{bd_props= upd(bd_props b)}

initialBlockData :: Sampler -> BlockTiming -> BlockNo -> SlotNo -> BlockData
initialBlockData shost bt b s =
  BlockData{ bd_props = defaultBlockProps b s,
             bd_timing= Map.singleton shost bt
           }

emptyBlockTiming :: BlockTiming
emptyBlockTiming =
  BlockTiming {
    bt_downloadedHeader = Map.empty,
    bt_sendFetchRequest = mempty,
    bt_completedBlockFetch = mempty,
    bt_addedToCurrentChain = Nothing
  }

initialBlockTiming :: Peer -> UTCTime -> BlockTiming
initialBlockTiming peer time =
  emptyBlockTiming {bt_downloadedHeader = Map.singleton peer time}

defaultBlockProps :: BlockNo -> SlotNo -> BlockProps
defaultBlockProps b s =
  BlockProps {
    bp_blockNo = b,
    bp_slot    = s,
    bp_size    = Nothing
  }

--------------------------------------------------------------------------------
-- BlockState

-- | A collection of `BlockData` values, keyed by `Hash`es
--
-- Invariant:
--   size of the Map <= blockStateMax

newtype BlockState = BlockState (Map Hash BlockData)
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (Monoid, Semigroup)
  deriving anyclass (ToJSON)

-- FIXME[F2]: See Marcin's review comments re Hash, add comments & justifications

-- | the maximum size of the Map in BlockState
blockStateMax :: Int
blockStateMax = 5
  -- FIXME: make this configurable.

sortBySlot :: BlockState -> [(Hash, BlockData)]
sortBySlot (BlockState m) =
  sortOn (Down . bp_slot . bd_props . snd) (Map.toList m)

addSeenHeader :: Sampler -> SlotNo -> BlockNo -> Hash -> Peer -> UTCTime -> BlockState -> IO (Maybe BlockState)
addSeenHeader shost slot block hash peer time (BlockState m) =
  return $ Just $
    BlockState (Map.alter alterBlockData hash m)
  where
    alterBlockData = \case
      -- 'hash' not in 'm':
      Nothing -> Just (initialBlockData
                         shost
                         (initialBlockTiming peer time)
                         block slot)
      -- 'hash' exists in 'm':
      Just bd -> Just $
        updateBlockTimingForSampler shost
          (\bt->bt{bt_downloadedHeader =
            Map.insertWith
              (unexpectedExisting "addSeenHeader: unexpected entry")
                 -- FIXME: turn into Left
              peer
              time
              (bt_downloadedHeader bt)})
          bd

addFetchRequest :: Sampler
                -> Int
                -> Peer
                -> UTCTime
                -> BlockData
                -> Possibly BlockData
addFetchRequest shost _len peer time =
    Right
  . updateBlockTimingForSampler shost
    (\bt->bt{bt_sendFetchRequest=
       Map.insertWith
         (unexpectedExisting "addFetchRequest")   -- FIXME: turn into Left
         peer
         time
         (bt_sendFetchRequest bt)})
  -- what is _len?

addFetchCompleted :: Sampler
                  -> Peer
                  -> UTCTime
                  -> BlockData
                  -> Possibly BlockData
addFetchCompleted shost peer time =
    Right
  . updateBlockTimingForSampler shost
    (\bt->bt{bt_completedBlockFetch=
       Map.insertWith
         (unexpectedExisting "addFetchCompleted") -- FIXME: turn into Left
         peer
         time
         (bt_completedBlockFetch bt)})

addAddedToCurrentChain
  :: Sampler
  -> StrictMaybe Int
  -> Int
  -> UTCTime
  -> BlockData
  -> Possibly BlockData
addAddedToCurrentChain shost msize _len time =
    Right   -- possible errors in future.
  . updateBlockTimingForSampler shost
      (\bt->bt{ bt_addedToCurrentChain = Just time})
  . updateBlockProps  (\bp->bp{ bp_size = strictMaybeToMaybe msize})
  -- FIXME: msize vs. _len?? [in current testing: always Nothing]

--------------------------------------------------------------------------------
-- BlockStateHdl

-- | A handle to a mutable `BlockState` object
newtype BlockStateHdl = BlockStateHdl (IORef BlockState)

newBlockStateHdl :: IO BlockStateHdl
newBlockStateHdl = BlockStateHdl <$> newIORef mempty

readBlockStateHdl :: BlockStateHdl -> IO BlockState
readBlockStateHdl (BlockStateHdl ref) = readIORef ref

updateBlockState :: BlockStateHdl -> (BlockState -> BlockState) -> IO ()
updateBlockState (BlockStateHdl ref) = modifyIORef' ref

updateBlockStateMaybe' :: BlockStateHdl
                       -> (BlockState -> Either a BlockState)
                       -> IO (Maybe a)
updateBlockStateMaybe' (BlockStateHdl ref) =
  atomicModifyIORefMaybe' ref

updateBlockStateByKey :: BlockStateHdl
                      -> Hash
                      -> (BlockData -> Possibly BlockData)
                      -> IO ()
updateBlockStateByKey (BlockStateHdl ref) k f =
  modifyIORefIOMaybe ref update
  where
    update (BlockState m) =
      case adjustIfMember2 f k m of
        Right m' -> pure (Just (BlockState m'))
        Left ss  -> warnMsg ss >> pure Nothing

modifyBlockStateIOMaybe :: BlockStateHdl
                        -> (BlockState -> IO (Maybe BlockState))
                        -> IO ()
modifyBlockStateIOMaybe (BlockStateHdl ref) f =
  modifyIORefIOMaybe ref f

pruneOverflow :: BlockStateHdl -> IO [(Hash, BlockData)]
pruneOverflow (BlockStateHdl ref) = atomicModifyIORef' ref prune
  where
    prune (BlockState m) =
      let (keepers, overflow) =
             splitMapOn blockStateMax (bp_slot . bd_props) m
      in (BlockState keepers, overflow)

---- IORef utilities -------------------------------------------------

-- | atomicModifyIORefMaybe' - small variation on atomicModifyIORef'
atomicModifyIORefMaybe' :: IORef a -> (a -> Either b a) -> IO (Maybe b)
atomicModifyIORefMaybe' ref f = atomicModifyIORef' ref g
  where
  g a = case f a of
          Right a' -> (a', Nothing)
          Left  b  -> (a , Just b)


modifyIORefIOMaybe :: IORef a -> (a -> IO (Maybe a)) -> IO ()
modifyIORefIOMaybe ref f =
  do
    a <- readIORef ref
    ma <- f a
    case ma of
      Just a' -> writeIORef ref a'
      Nothing -> pure ()
  -- NOTE: not re-entrant

---- Map utilities -------------------------------------------------

-- | Reduce size of `Map` `m` to `keep` elements, using `order` to order
-- elements. The "greatest" elements, according to `f`, are kept.
splitMapOn :: (Ord k, Ord v2) => Int -> (v1 -> v2) -> Map k v1 -> (Map k v1, [(k, v1)])
splitMapOn keep order m = (Map.fromList keepers, overflow)
  where
    (keepers, overflow) =
      splitAt keep $
        sortOn (Down . order . snd) $
          Map.toList m

-- | Use with `Map.insertWith` to trigger an error when a value would be updated
-- on insertion
unexpectedExisting :: String -> any -> any -> any
unexpectedExisting msg _new _old = error msg

-- | Adjust the value by the function if the key exists, or produce Nothing
-- otherwise.
--
-- >>> adjustIfMember (+ 1) "one" (Map.singleton "two" 2)
-- Nothing
--
-- >>> adjustIfMember (+ 1) "one" (Map.singleton "one" 1)
-- Just (Map.fromList [("one", 2)])
adjustIfMember :: Ord k => (a -> a) -> k -> Map k a -> Maybe (Map k a)
adjustIfMember f = Map.alterF (fmap (Just . f))


-- | similar to the last but ... (FIXME: doc!)
adjustIfMember2 :: (Show k, Ord k)
                => (a -> Possibly a)
                -> k -> Map k a -> Possibly (Map k a)
adjustIfMember2 f k =
  Map.alterF
    (\case
        Nothing -> Left ["key '" ++ show k ++"' not in Map"]
        Just a  -> fmap Just (f a))
    k
