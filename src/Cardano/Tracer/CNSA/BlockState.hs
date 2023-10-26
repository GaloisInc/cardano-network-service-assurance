{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    defaultBlockData,
    sortBySlot,
    addSeenHeader,
    addFetchRequest,
    addFetchCompleted,
    addAddedToCurrentChain,
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

-- <<<<<<< HEAD
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
import           Data.Maybe.Strict (StrictMaybe, strictMaybeToMaybe)
import           Data.Ord (Down (..))
import           Data.Time (UTCTime)
import           GHC.Generics (Generic)


--------------------------------------------------------------------------------
-- BlockData

data BlockData = BlockData
  { bd_props  :: BlockProps,              -- ^ constants for a given block.
    bd_timing :: BlockTiming              -- ^ timing info for each sampling
                                          --   invariant: map not empty.
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

updateBlockTiming :: (BlockTiming -> BlockTiming) -> BlockData -> BlockData
updateBlockTiming upd b = b{bd_timing= upd(bd_timing b)}

updateBlockProps :: (BlockProps -> BlockProps) -> BlockData -> BlockData
updateBlockProps upd b = b{bd_props= upd(bd_props b)}

defaultBlockData :: BlockNo -> SlotNo -> BlockData
defaultBlockData b s =
  BlockData{ bd_props = defaultBlockProps b s,
             bd_timing= defaultBlockTiming
           }

defaultBlockTiming :: BlockTiming
defaultBlockTiming =
  BlockTiming {
    bt_downloadedHeader = mempty,
    bt_sendFetchRequest = mempty,
    bt_completedBlockFetch = mempty,
    bt_addedToCurrentChain = Nothing
  }

defaultBlockProps :: BlockNo -> SlotNo -> BlockProps
defaultBlockProps b s =
  BlockProps {
    bp_blockNo = b,
    bp_slot    = s,
    bp_size   = Nothing
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
-- FIXME[F1]: add the address of the sample node we received from!!

-- | the maximum size of the Map in BlockState
blockStateMax :: Int
blockStateMax = 5
  -- FIXME: make this configurable.

sortBySlot :: BlockState -> [(Hash, BlockData)]
sortBySlot (BlockState m) =
  sortOn (Down . bp_slot . bd_props . snd) (Map.toList m)

addSeenHeader :: Sampler -> SlotNo -> BlockNo -> Hash -> Peer -> UTCTime -> BlockState -> BlockState
addSeenHeader _shost slot block hash peer time (BlockState m) =
  BlockState (Map.alter f hash m)
  where
    update =
      updateBlockTiming
        (\bt->bt{bt_downloadedHeader =
          Map.insertWith
            (unexpectedExisting "addSeenHeader: unexpected entry")
            peer
            time
            (bt_downloadedHeader bt)})
    f blockDataM =
      case blockDataM of
        Nothing -> Just (update (defaultBlockData block slot))
        Just bd -> Just (update bd)

addFetchRequest :: Sampler
                -> Int
                -> Peer
                -> UTCTime
                -> BlockData -> BlockData
addFetchRequest _shost _len peer time =
  updateBlockTiming $
   \bt->bt{bt_sendFetchRequest=
    Map.insertWith
      (unexpectedExisting "addFetchRequest")
      peer
      time
      (bt_sendFetchRequest bt)}
  -- what is _len?

addFetchCompleted :: Sampler -> Peer -> UTCTime -> BlockData -> BlockData
addFetchCompleted _shost peer time =
  updateBlockTiming $
    \bt->bt{bt_completedBlockFetch=
       Map.insertWith
      (unexpectedExisting "addFetchCompleted")
      peer
      time
      (bt_completedBlockFetch bt)}

addAddedToCurrentChain
  :: Sampler
  -> StrictMaybe Int
  -> Int
  -> UTCTime
  -> BlockData -> BlockData
addAddedToCurrentChain shost msize _len time =
    updateBlockTiming (\bt->bt{ bt_addedToCurrentChain = Just time})
  . updateBlockProps  (\bp->bp{ bp_size = strictMaybeToMaybe msize})
  -- FIXME: msize vs. _len?? [in current testing: always Nothing]
  -- why can you ignore _hash?

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

updateBlockStateByKey :: BlockStateHdl -> Hash -> (BlockData -> BlockData) -> IO ()
updateBlockStateByKey (BlockStateHdl ref) k f = modifyIORefMaybe ref update
  where
    update (BlockState m) =
      case adjustIfMember f k m of
        Just m' -> pure (Just (BlockState m'))
        Nothing ->
          do
            warnMsg ["ignoring TraceObj: can't update unknown hash (either tool old or error): " ++ show k]
            pure Nothing

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


modifyIORefMaybe :: IORef a -> (a -> IO (Maybe a)) -> IO ()
modifyIORefMaybe ref f =
  do
    a <- readIORef ref
    ma <- f a
    case ma of
      Just a' -> writeIORef ref a'
      Nothing -> pure ()

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

-- | Adjust the value by the function if the key exists, or produce `Nothing`
-- otherwise.
--
-- >>> adjustIfMember (+ 1) "one" (Map.singleton "two" 2)
-- Nothing
--
-- >>> adjustIfMember (+ 1) "one" (Map.singleton "one" 1)
-- Just (Map.fromList [("one", 2)])
adjustIfMember :: Ord k => (a -> a) -> k -> Map k a -> Maybe (Map k a)
adjustIfMember f = Map.alterF (fmap (Just . f))
