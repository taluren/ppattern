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
      aux _ 0 _ = [[]]
      aux _ h 1 = [[h]]
      aux l h k = [x:xs | x <- [l..(h-k+1)], xs <- aux x (h-x) (k-1)]

      upToIsomorphism :: [[Int]] -> [[Int]]
      upToIsomorphism = Set.toList . Set.fromList . L.map (L.sortBy (flip compare))
