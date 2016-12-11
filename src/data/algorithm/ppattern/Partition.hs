{-|
Module      : Data.Algorithm.PPattern.Partition
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Partition
(
  IntPartition
  --
, fromList
  --
, partitions
, partitionsByLength
)
where

  import qualified Data.List as L
  import qualified Data.Set  as Set

  newtype IntPartition a = IntPartition { toList :: [a] }
    deriving (Eq, Show, Ord)

  fromList :: (Ord a) => [a] -> IntPartition a
  fromList  = IntPartition . L.sortBy (flip compare)

  partitions :: (Enum a, Num a, Ord a) => a -> [IntPartition a]
  partitions n = upToIsomorphism . L.map fromList $ aux 1 n
    where
      aux _ 0 = [[]]
      aux l h = [x:xs | x <- [l..h], xs <- aux x (h-x)]

      upToIsomorphism = L.sort . Set.toList . Set.fromList

  partitionsByLength :: (Enum a, Num a, Ord a) => a -> a -> [IntPartition a]
  partitionsByLength n k = upToIsomorphism . L.map fromList $ aux 1 n k
    where
      aux _ 0 _ = [[]]
      aux _ h 1 = [[h]]
      aux l h k = [x:xs | x <- [l..(h-k+1)], xs <- aux x (h-x) (k-1)]

      upToIsomorphism = L.sort . Set.toList . Set.fromList
