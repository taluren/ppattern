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
-- , increasingSubsequences
-- , partitionsIncreasingsByLength
)
where

  import qualified Data.List     as L
  import qualified Data.Foldable as F
  import qualified Data.Tuple    as T
  import qualified Data.Set      as Set

  import qualified Data.Algorithm.PPattern.IntPartition as IntPartition
  import qualified Data.Algorithm.PPattern.Seq          as Seq

  {-|
    The 'square' function squares an integer.
    It takes one argument, of type 'Int'.
  -}
  increasingSubsequences :: (Ord a) => [a] ->  Int -> [[a]]
  increasingSubsequences [] _ = [[]]
  increasingSubsequences xs l = aux xs l z
    where
      aux _      0 _  = [[]]
      aux []     _ _  = []
      aux (x:xs) l' x'
        | l == l' || x > x' = L.map (x:) (aux xs (l'-1) x) ++ aux xs l' x'
        | otherwise         = aux xs l' x'

      z = L.head xs

  {-|
    The 'square' function squares an integer.
    It takes one argument, of type 'Int'.
  -}
  partitionsIncreasingsByLength :: (Ord a) => [a] -> [Int] -> [[[a]]]
  partitionsIncreasingsByLength [] []     = [[]]
  partitionsIncreasingsByLength [] _      = []
  partitionsIncreasingsByLength _  []     = []
  partitionsIncreasingsByLength xs (l:ls) = ps
    where
      ps = [is:iss | is  <- increasingSubsequences xs l,
                     iss <- partitionsIncreasingsByLength (xs L.\\ is) ls]

  {-|
    The 'square' function squares an integer.
    It takes one argument, of type 'Int'.
  -}
  partitionsIncreasings :: (Ord a) => Seq.Seq t a -> Int -> [[Seq.Seq Seq.Isogram a]]
  partitionsIncreasings (Seq.Seq xs) k = L.map (L.map Seq.fromList) . upToIsomorphism $ aux xs k
    where
      aux xs k = L.concat [partitionsIncreasingsByLength xs (IntPartition.toList p) |
                           p <- IntPartition.partitionsByLength (L.length xs) k]

      upToIsomorphism = Set.toList . Set.fromList . L.map L.sort
