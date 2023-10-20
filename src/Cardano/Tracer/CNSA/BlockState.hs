{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use forM_" #-}

module Cardano.Tracer.CNSA.BlockState
  ( BlockData (..),
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
  { bl_blockNo :: BlockNo,
    bl_slot :: SlotNo,
    bl_downloadedHeader :: [(Peer, UTCTime)],
    bl_sendFetchRequest :: [(Peer, UTCTime)],
    bl_completedBlockFetch :: [(Peer, UTCTime)],
    -- KK: CompletedBlockFetch*, this trace is in the wrong
    -- place. We need a trace for when the block has been
    -- downloaded, see #4226.
    bl_addedToCurrentChain :: Maybe UTCTime,
    bl_size :: Maybe Int
  }
  deriving (Eq, Ord, Show, Generic, ToJSON)

defaultBlockData :: BlockNo -> SlotNo -> BlockData
defaultBlockData b s =
  BlockData
    { bl_blockNo = b,
      bl_slot = s,
      bl_downloadedHeader = [],
      bl_sendFetchRequest = [],
      bl_completedBlockFetch = [],
      bl_addedToCurrentChain = Nothing,
      bl_size = Nothing
    }

--------------------------------------------------------------------------------
-- BlockState

-- | A collection of `BlockData` values, keyed by `Hash`es
newtype BlockState = BlockState (Map Hash BlockData)
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (Monoid, Semigroup)
  deriving anyclass (ToJSON)

-- FIXME[F2]: See Marcin's review comments re Hash
-- FIXME[F1]: add the address of the sample node we received from!!
--   BTW, another useful view might be
--     Map SlotNo (Map Hash [(Addr,BlockData)])

-- FIXME: make configurable.
blockStateMax :: Int
blockStateMax = 5

sortBySlot :: BlockState -> [(Hash, BlockData)]
sortBySlot (BlockState m) = sortOn (Down . bl_slot . snd) (Map.toList m)

addSeenHeader :: SlotNo -> BlockNo -> Hash -> Peer -> UTCTime -> BlockState -> BlockState
addSeenHeader slot block hash peer time (BlockState m) = BlockState (Map.alter f hash m)
  where
    update bd = bd {bl_downloadedHeader = bl_downloadedHeader bd ++ [(peer, time)]}
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
            warnMsg ["ignoring Log, hash not in current block data: " ++ show k]
            pure Nothing

pruneOverflow :: BlockStateHdl -> IO [(Hash, BlockData)]
pruneOverflow (BlockStateHdl ref) = atomicModifyIORef' ref prune
  where
    prune (BlockState m) =
      let (keepers, overflow) = splitMapOn blockStateMax bl_slot m
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
