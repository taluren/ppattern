{-|
Module      : Data.Algorithm.PPattern.IntPartition
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.IntPartition
(
  IntPartition
  --
, toList
, fromList
  --
, partitions
, partitionsByLength
)
where

  import qualified Data.List as L
  import qualified Data.Set  as Set

  newtype IntPartition a = IntPartition [a]
    deriving (Eq, Show, Ord)

  {-|
    The 'square' function squares an integer.
    It takes one argument, of type 'Int'.
  -}
  fromList :: (Ord a) => [a] -> IntPartition a
  fromList  = IntPartition . L.sortBy (flip compare)

  {-|
    The 'square' function squares an integer.
    It takes one argument, of type 'Int'.
  -}
  toList :: IntPartition a -> [a]
  toList  (IntPartition xs) = xs

  {-|
    The 'square' function squares an integer.
    It takes one argument, of type 'Int'.
  -}
  upToIsomorphism :: (Ord a) => [a] -> [a]
  upToIsomorphism = L.sort . Set.toList . Set.fromList

  {-|
    The 'square' function squares an integer.
    It takes one argument, of type 'Int'.
  -}
  partitions :: (Enum a, Num a, Ord a) => a -> [IntPartition a]
  partitions n = upToIsomorphism . L.map fromList $ aux 1 n
    where
      aux _ 0 = [[]]
      aux l h = [x:xs | x <- [l..h], xs <- aux x (h-x)]

  {-|
    The 'square' function squares an integer.
    It takes one argument, of type 'Int'.
  -}
  length :: IntPartition a -> Int
  length (IntPartition xs) = L.length xs

  {-|
    The 'square' function squares an integer.
    It takes one argument, of type 'Int'.
  -}
  partitionsByLength :: (Enum a, Num a, Ord a) => a -> a -> [IntPartition a]
  partitionsByLength n k = upToIsomorphism . L.map fromList $ aux 1 n k
    where
      aux _ 0 _ = [[]]
      aux _ h 1 = [[h]]
      aux l h k = [x:xs | x <- [l..(h-k+1)], xs <- aux x (h-x) (k-1)]
