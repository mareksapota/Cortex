module Adria.Common.Util
    ( stablePartition
    , stableFilter
    ) where

-----
-- Version of partition that preserves order of equal elements.

stablePartition :: (a -> Bool) -> [a] -> ([a], [a])
stablePartition f l = stablePartition' f l ([], [])

stablePartition' :: (a -> Bool) -> [a] -> ([a], [a]) -> ([a], [a])
stablePartition' f [] (a, b) = (reverse a, reverse b)
stablePartition' f (h:t) (a, b)
    | f h = stablePartition' f t (h:a, b)
    | otherwise = stablePartition' f t (a, h:b)

-----
-- Version of filter that preserves order of equal elements.

stableFilter :: (a -> Bool) -> [a] -> [a]
stableFilter f l = fst $ stablePartition f l

-----
