{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- | utilities and abstractions over Data.Map.Strict

module Util.Data.Map where

import           Data.List (sortOn)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Ord (Down (..))

-- | Reduce size of `Map` `m` to `keep` elements, using `order` to order
-- elements. The "greatest" elements, according to `f`, are kept.
splitMapOn :: (Ord k, Ord v2) => Int -> (v1 -> v2) -> Map k v1 -> (Map k v1, [(k, v1)])
splitMapOn keep order m = (Map.fromList keepers, overflow)
  where
    (keepers, overflow) =
      splitAt keep $
        sortOn (Down . order . snd) $
          Map.toList m

-- | Adjust the value by the adjust function if the key exists, or produce
-- Left otherwise.
adjustIfMember :: (Show k, Ord k)
               => (a -> Either [String] a)
               -> k -> Map k a -> Either [String] (Map k a)
adjustIfMember f k =
  Map.alterF
    (\case
        Nothing -> Left ["key '" ++ show k ++"' not in Map"]
        Just a  -> fmap Just (f a))
    k

-- | Collect the values found at each key in the input maps. The order of the
-- values in the result is the same as their order in the input.
--
-- In other words, `catMaybes (map (\m -> m Map.!? k) ms) == mapSequence ms
-- Map.! k`, assuming `k` is in at least one of the input maps.
mapSequence :: Ord k => [Map k v] -> Map k [v]
mapSequence maps = (\xs -> xs []) <$> Map.unionsWith (.) [fmap (:) m | m <- maps]

-- | `k` maps to `v / n` in the output iff `k` maps to `n` `v`s across the
-- inputs, where `n > 0`. If `n == 0`, `k` does not exist in the output.
averageByKey :: (Ord k, Fractional v) => [Map k v] -> Map k v
averageByKey maps = fmap divide (Map.unionsWith add counted)
  where
    counted = map (fmap (, 1)) maps
    add (diff1, count1) (diff2, count2) = (diff1 + diff2, count1 + count2)
    divide (diff, count) = diff / count

-- | In the output, `k1` maps to the `averageByKey` of its values across the
-- input.
multiAverageByKey :: (Ord k1, Ord k2, Fractional v) => [Map k1 (Map k2 v)] -> Map k1 (Map k2 v)
multiAverageByKey delays = fmap averageByKey (mapSequence delays)

