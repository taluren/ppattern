{-|
Module      : Data.Algorithm.PPattern.Permutation.Splitting
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Permutation.Splitting
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
  import qualified Data.Foldable as Fold
  import qualified Data.Tuple    as T
  import qualified Data.Set      as Set

  import qualified Data.Algorithm.PPattern.Permutation  as Permutation
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
  partitionsIncreasingsByL :: Permutation.Permutation -> IntPartition.IntPartition -> [[Permutation.Permutation]]
  partitionsIncreasingsByL p intPartition = aux xs ls
    where
      xs = Permutation.toList p
      ls = IntPartition.toList intPartition

      aux [] []     = [[]]
      aux [] _      = []
      aux _  []     = []
      aux xs (l:ls) = fmap (fmap Permutation.fromListUnsafe) ps
        where
          ps = [is:iss | is  <- increasingsByL xs l, iss <- aux (xs L.\\ is) ls]

  {-|
    'isClassLeader xss' returns 'True' if and only if xss is composed of
    ascending sorted list, each list being sorted ascending.
  -}
  isClassLeader :: [Permutation.Permutation] -> Bool
  isClassLeader ps = xss == xss'
    where
      xss = [Permutation.toList p | p <- ps]
      xss' = L.sort $ L.map L.sort xs

  {-|
    'partitionsIncreasings p n' return all partitions of permutation 'p' into 'k'
    increasing subsequences.
  -}
  partitionsIncreasings :: Permutation -> Int -> [[Permutation]]
  partitionsIncreasings p k
    | Stringology.lenLongestDecreasingSub p > k = []
    | otherwise                                 = aux xs k
    where
      aux xs k = [fmap Permutation.fromListUnsafe ip' |
                  intPartition <- IntPartition.intPartitionsByL n k,
                  pPartition   <- partitionsIncreasingsByL p intPartition,
                  isClassLeader psPartition]
      n = Permutation.length p

  {-|
    'greedyPartitionIncreasings xs f' return a partition of xs into increasing
    subsequences by repeatidily calling function 'f' on the remaining subsequence.
  -}
  greedyPartitionIncreasings :: (Permutation.Permutation -> Permutation.Permutation) -> Permutation.Permutation -> [Permutation.Permutation]
  greedyPartitionIncreasings f (Permutation.Permutation xs) = aux [] xs
    where
      aux acc [] = Permutation.fromListUnsafe acc
      aux acc xs = aux (xs':acc) (xs L.\\ xs')
        where
          xs' = f xs

  {-|
    'greedyPartitionIncreasings1' takes a list 'xs'. It greedily computes a partition
    of 'xs' into increasing subsequences.
  -}
  greedyPartitionIncreasings1 :: Permutation.Permutation -> [Permutation.Permutation]
  greedyPartitionIncreasings1 = greedyPartitionIncreasings greedyIncreasing1

  {-|
    'greedyIncreasing1' takes a list 'xs'. It greedily computes an increasing
    subsequence of 'xs'.
  -}
  greedyIncreasing1 :: Permutation.Permutation -> Permutation.Permutation
  greedyIncreasing1 (Permutation.Permutation [])     = Permutation.empty
  greedyIncreasing1 (Permutation.Permutation (x:xs)) = Permutation.fromListUnsafe $ x:aux x xs
    where
      aux _ []      = []
      aux x (x':xs)
        | x' > x    = x':aux x' xs
        | otherwise =    aux x  xs

  {-|
    'greedyPartitionIncreasings1' takes a list 'xs'. It greedily computes a partition
    of 'xs' into increasing subsequences.
  -}
  greedyPartitionIncreasings2 :: Permutation.Permutation -> [Permutation.Permutation]
  greedyPartitionIncreasings2 = greedyPartitionIncreasings greedyIncreasing2


  {-|
    'greedyIncreasing1' takes a list 'xs'. It greedily computes an increasing
    subsequence of 'xs'.
  -}
  greedyIncreasing2 :: Permutation.Permutation -> Permutation.Permutation
  greedyIncreasing2 = Stringology.longestIncreasingSub
