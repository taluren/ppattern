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
, partitions
, nbIntPartitions
, partitionsL
, nbpartitionsL

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
  partitions :: Int -> [IntPartition]
  partitions n = L.concat [partitionsL n k | k <- [1..n]]

  {-|
    'nbIntPartitions n' returns the number of ordered partitions of integer 'n'.
  -}
  nbIntPartitions :: Int -> Int
  nbIntPartitions = L.length . partitions

  {-|
    'partitionsL n k' returns all ordered partitions of integer 'n' into 'k'
    parts.

    λ: partitionsL 6 0
    [[]]
    λ: partitionsL 6 1
    [[6]]
    λ: partitionsL 6 2
    [[3,3],[4,2],[5,1]]
    λ: partitionsL 6 3
    [[2,2,2],[3,2,1],[4,1,1]]
    λ: partitionsL 6 4
    [[2,2,1,1],[3,1,1,1]]
    λ: partitionsL 6 5
    [[2,1,1,1,1]]
    λ: partitionsL 6 6
    [[1,1,1,1,1,1]]
    λ: partitionsL 6 7
    []
  -}
  partitionsL :: Int -> Int -> [IntPartition]
  partitionsL n k = fromList <$> partitionsLAux n k n

  partitionsLAux :: Int -> Int -> Int -> [[Int]]
  partitionsLAux _ 0 _ = [[]]
  partitionsLAux n 1 _ = [[n]]
  partitionsLAux n k b
    | n < k     = []
    | n == k    = [L.replicate k 1]
    | otherwise = L.concat [fmap (k':) (partitionsLAux (n-k') (k-1) k') |
                            k' <- [l..h]]
    where
      l = fromIntegral (ceiling ((fromIntegral n / fromIntegral k) :: Double) :: Int)
      h = min (n-k+1) b

  {-|
    'nbpartitionsL n k' returns the number of ordered partitions of integer
    'n' into 'k' parts.
  -}
  nbpartitionsL :: Int -> Int -> Int
  nbpartitionsL n k = L.length $ partitionsL n k

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
