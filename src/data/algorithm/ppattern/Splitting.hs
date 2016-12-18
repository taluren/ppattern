{-|
Module      : Data.Algorithm.PPattern.Splitting
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Data.Algorithm.PPattern.Splitting
(
  partitionsIncreasings
  --
-- , increasingsByL
-- , partitionsIncreasingsByL
, greedyIncreasing1
, greedyPartitionIncreasings1
, greedyIncreasing2
, greedyPartitionIncreasings2
)
where

  import qualified Data.List     as L
  import qualified Data.Foldable as F
  import qualified Data.Tuple    as T
  import qualified Data.Set      as Set

  import qualified Data.Algorithm.PPattern.IntPartition as IntPartition
  import qualified Data.Algorithm.PPattern.LCS          as LCS

  {-|
    'increasingsByL xs k' return the list of all increasing subsequences
    of length 'k' of 'xs'.
  -}
  increasingsByL :: (Ord a) => [a] ->  Int -> [[a]]
  increasingsByL [] _ = [[]]
  increasingsByL xs k = aux xs k (L.head xs)
    where
      aux _      0 _  = [[]]
      aux []     _ _  = []
      aux (x:xs) k' x'
        | k == k' || x > x' = fmap (x:) (aux xs (k'-1) x) ++ aux xs k' x'
        | otherwise         = aux xs k' x'

  {-|
    'partitionsIncreasingsByL xs ks' returns all partitions of 'xs' into |ks|
    increasing subsequences of length ks = [k1, k2, ..., kp].
  -}
  partitionsIncreasingsByL :: (Ord a) => [a] -> [Int] -> [[[a]]]
  partitionsIncreasingsByL [] []     = [[]]
  partitionsIncreasingsByL [] _      = []
  partitionsIncreasingsByL _  []     = []
  partitionsIncreasingsByL xs (l:ls) = ps
    where
      ps = [is:iss | is  <- increasingsByL xs l,
                     iss <- partitionsIncreasingsByL (xs L.\\ is) ls]

  {-|
    'isClassLeader xss' returns 'True' if and only if xss is composed of
    ascending sorted list, each list being sorted ascending.
  -}
  isClassLeader :: [[Int]] -> Bool
  isClassLeader xs = xs == xs'
    where
      xs' = L.sort $ L.map L.sort xs

  {-|
    'partitionsIncreasings xs n' return all partitions of 'xs' into 'k'
    increasing subsequences.
  -}
  partitionsIncreasings :: [Int] -> Int -> [[[Int]]]
  partitionsIncreasings xs k
    | LCS.lenLongestDecreasingSub xs > k = []
    | otherwise                          = aux xs k
    where
      aux xs k = [ip' | ip  <- IntPartition.intPartitionsByL (L.length xs) k,
                        ip' <- partitionsIncreasingsByL xs ip,
                        isClassLeader ip']


  {-|
    'greedyPartitionIncreasings xs f' return a partition of xs into increasing
    subsequences by repeatidily calling function 'f' on the remaining subsequence.
  -}
  greedyPartitionIncreasings :: [Int] -> ([Int] -> [Int]) -> [[Int]]
  greedyPartitionIncreasings xs f = aux [] xs
    where
      aux acc [] = acc
      aux acc xs = aux (xs':acc) (xs L.\\ xs')
        where
          xs' = f xs

  {-|
    'greedyPartitionIncreasings1' takes a list 'xs'. It greedily computes a partition
    of 'xs' into increasing subsequences.
  -}
  greedyPartitionIncreasings1 :: [Int] -> [[Int]]
  greedyPartitionIncreasings1 xs = greedyPartitionIncreasings xs greedyIncreasing1

  {-|
    'greedyIncreasing1' takes a list 'xs'. It greedily computes an increasing
    subsequence of 'xs'.
  -}
  greedyIncreasing1 :: [Int] -> [Int]
  greedyIncreasing1 []     = []
  greedyIncreasing1 (x:xs) = x:aux x xs
    where
      aux _ []      = []
      aux x (x':xs)
        | x' > x    = x':aux x' xs
        | otherwise =    aux x  xs

  {-|
    'greedyPartitionIncreasings1' takes a list 'xs'. It greedily computes a partition
    of 'xs' into increasing subsequences.
  -}
  greedyPartitionIncreasings2 :: [Int] -> [[Int]]
  greedyPartitionIncreasings2 xs = greedyPartitionIncreasings xs greedyIncreasing2


  {-|
    'greedyIncreasing1' takes a list 'xs'. It greedily computes an increasing
    subsequence of 'xs'.
  -}
  greedyIncreasing2 :: [Int] -> [Int]
  greedyIncreasing2 = LCS.longestIncreasingSub
