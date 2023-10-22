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
    updateBlockTiming,
    updateBlockProps,
    BlockState,
    sortBySlot,
    addSeenHeader,
    BlockStateHdl,
    newBlockStateHdl,
    readBlockStateHdl,
    updateBlockState,
    updateBlockStateByKey,
    pruneOverflow,
  )
where

{-
Secrets kept:
- `BlockState` is a `Map`
- `BlockStateHdl` is an `IORef`
-}

import Cardano.Analysis.API.Ground (Hash)
import Cardano.Slotting.Block (BlockNo)
import Cardano.Slotting.Slot (SlotNo)
import Cardano.Tracer.CNSA.ParseLogs (Peer)
import Cardano.Utils.Log (warnMsg)
import Data.Aeson (ToJSON)
import Data.IORef
  ( IORef,
    atomicModifyIORef',
    modifyIORef',
    newIORef,
    readIORef,
    writeIORef,
  )
import Data.List (sortOn)
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import Data.Ord (Down (..))
import Data.Time (UTCTime)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- BlockData

data BlockData = BlockData
  { bd_props  :: BlockProps,  -- ^ constants for a given block.
    bd_timing :: BlockTiming  -- ^ timing info
  }
  deriving (Eq, Ord, Show, Generic, ToJSON)

-- | data intrinsic to a block.
data BlockProps = BlockProps
  { bp_blockNo :: BlockNo,
    bp_slot    :: SlotNo,
    bp_size    :: Maybe Int
  }
  deriving (Eq, Ord, Show, Generic, ToJSON)

-- | block timing data with respect to a sampling node [and its peers]:
data BlockTiming = BlockTiming
  { bt_downloadedHeader    :: [(Peer, UTCTime)],
    bt_sendFetchRequest    :: [(Peer, UTCTime)],
    bt_completedBlockFetch :: [(Peer, UTCTime)],
      -- KK: CompletedBlockFetch*, this trace is in the wrong
      -- place. We need a trace for when the block has been
      -- downloaded, see #4226.
    bt_addedToCurrentChain :: Maybe UTCTime
  }
  deriving (Eq, Ord, Show, Generic, ToJSON)

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
    bt_downloadedHeader = [],
    bt_sendFetchRequest = [],
    bt_completedBlockFetch = [],
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

addSeenHeader :: SlotNo -> BlockNo -> Hash -> Peer -> UTCTime -> BlockState -> BlockState
addSeenHeader slot block hash peer time (BlockState m) =
  BlockState (Map.alter f hash m)
  where
    update =
      updateBlockTiming
        (\bt->bt{bt_downloadedHeader = bt_downloadedHeader bt ++ [(peer, time)]})
    f blockDataM =
      case blockDataM of
        Nothing -> Just (update (defaultBlockData block slot))
        Just bd -> Just (update bd)

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
