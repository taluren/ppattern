{-|
Module      : Data.Algorithm.PPattern.Combinatorics
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Combinatorics
(
  partitionsByLength
)
where

  import qualified Data.List as L
  import qualified Data.Set  as Set

  {-|
    The 'partitionsByLength' function returns all k-partitions of an integer.
  -}
  partitionsByLength :: Int -> Int -> [[Int]]
  partitionsByLength n k = upToIsomorphism $ aux 1 n k
    where
      aux :: Int -> Int -> Int -> [[Int]]
      aux m 0 _ = [[]]
      aux m n 1 = [[n]]
      aux m n k = [x:xs | x <- [m..(n-k+1)], xs <- aux x (n-x) (k-1)]

      upToIsomorphism :: [[Int]] -> [[Int]]
      upToIsomorphism = Set.toList . Set.fromList . L.map (L.sortBy (flip compare))
