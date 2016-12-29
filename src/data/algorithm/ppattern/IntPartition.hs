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
  -- * The @IntPartition@ type
  IntPartition(..)
, fromList
, mkIntPartition

  -- * Querying
, nbParts

  -- * Transforming
, toList

  -- * Generating and counting
, intPartitions
, nbIntPartitions
, intPartitionsByL
, nbIntPartitionsByL

  -- * Comparing
, compatible
)
where

  import qualified Data.List as L

  -- | Partition of integer.
  newtype IntPartition = IntPartition [Int] deriving (Show, Eq, Ord)

  {-|

  -}
  fromList :: [Int] -> IntPartition
  fromList = IntPartition

  {-|

  -}
  mkIntPartition :: [Int] -> IntPartition
  mkIntPartition = fromList

  {-|

  -}
  nbParts :: IntPartition -> Int
  nbParts (IntPartition xs) = L.length xs

  {-|

  -}
  toList :: IntPartition -> [Int]
  toList (IntPartition xs) = xs

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
  intPartitionsByL n k = fromList <$> intPartitionsByLAux n k n

  intPartitionsByLAux :: Int -> Int -> Int -> [[Int]]
  intPartitionsByLAux _ 0 _ = [[]]
  intPartitionsByLAux n 1 _ = [[n]]
  intPartitionsByLAux n k b
    | n < k     = []
    | n == k    = [L.replicate k 1]
    | otherwise = L.concat [fmap (k':) (intPartitionsByLAux (n-k') (k-1) k') |
                            k' <- [l..h]]
    where
      l = fromIntegral (ceiling ((fromIntegral n / fromIntegral k) :: Double) :: Int)
      h = min (n-k+1) b

  {-|
    'nbIntPartitionsByL n k' returns the number of ordered partitions of integer
    'n' into 'k' parts.
  -}
  nbIntPartitionsByL :: Int -> Int -> Int
  nbIntPartitionsByL n k = L.length $ intPartitionsByL n k

  {-|
    'compatible ip1 ip2' returns True if and only if 'ip1' is a subpartition of
    'ip2'. Both 'ip1' and 'ip2' need to be reverse sorted
  -}
  compatible :: IntPartition -> IntPartition -> Bool
  compatible ip1 ip2 = aux (toList ip1) (toList ip2)
    where
      aux [] []     = True
      aux _  []     = False
      aux [] _      = True
      aux (x1:x1s) (x2:x2s)
        | x1 <= x2  = aux  x1s     x2s
        | otherwise = aux (x1:x1s) x2s
