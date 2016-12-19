{-|
Module      : Data.Algorithm.PPattern.IntPartition
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Integer partitions.
-}

module Data.Algorithm.PPattern.IntPartition
(
  intPartitions
, nbIntPartitions
  --
, intPartitionsByL
, nbIntPartitionsByL
)
where

  import qualified Data.List as L
  import Control.Applicative

  newtype IntPartition = IntPartition [Int]

  {-|
    'intPartitions n ' returns all ordered partitions of integer 'n'.

    λ: intPartitions 6
    [[6],[3,3],[4,2],[5,1],[2,2,2],[3,2,1],[4,1,1],[2,2,1,1],[3,1,1,1],[2,1,1,1,1],[1,1,1,1,1,1]]
  -}
  intPartitions :: Int -> [IntPartition]
  intPartitions n = L.concat [intPartitionsByL n k | k <- [1..n]]

  {-|
    'nbIntPartitions n' returns the number of ordered partitions of integer 'n'.
  -}
  nbIntPartitions :: Int -> Int
  nbIntPartitions = L.length . intPartitions

  {-|
    'intPartitionsByL n k' returns all ordered partitions of integer 'n' into 'k'
    parts.

    λ: intPartitionsByL 6 0
    [[]]
    λ: intPartitionsByL 6 1
    [[6]]
    λ: intPartitionsByL 6 2
    [[3,3],[4,2],[5,1]]
    λ: intPartitionsByL 6 3
    [[2,2,2],[3,2,1],[4,1,1]]
    λ: intPartitionsByL 6 4
    [[2,2,1,1],[3,1,1,1]]
    λ: intPartitionsByL 6 5
    [[2,1,1,1,1]]
    λ: intPartitionsByL 6 6
    [[1,1,1,1,1,1]]
    λ: intPartitionsByL 6 7
    []
  -}
  intPartitionsByL :: Int -> Int -> [IntPartition]
  intPartitionsByL n k = IntPartition <$> aux n k n
    where
      aux _ 0 _ = [[]]
      aux n 1 _ = [[n]]
      aux n k b
        | n < k           = []
        | n == k          = [L.replicate k 1]
        | otherwise       = L.concat [fmap (k':) (aux (n-k') (k-1) k') |
                                      k' <- [l..h]]
        where
          l = fromIntegral (ceiling (fromIntegral n / fromIntegral k))
          h = min (n-k+1) b

  {-|
    'nbIntPartitionsByL n k' returns the number of ordered partitions of integer
    'n' into 'k' parts.
  -}
  nbIntPartitionsByL :: Int -> Int -> Int
  nbIntPartitionsByL n k = L.length $ intPartitionsByL n k
