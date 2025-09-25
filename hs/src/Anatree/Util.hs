-- | Helper functions for the implementation of the Anagram tree module.
module Anatree.Util where
import qualified Data.Ord as Ord

-- | Merge two lists following a certain ordering.
mergeBy :: (a -> a -> Ord.Ordering) -> [a] -> [a] -> [a]
mergeBy _   []     ys = ys
mergeBy _   xs     [] = xs
mergeBy cmp (x:xs) (y:ys) = case cmp x y of
  Ord.LT -> x : mergeBy cmp    xs  (y:ys)
  _      -> y : mergeBy cmp (x:xs)    ys

