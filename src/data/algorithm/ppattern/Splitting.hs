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

module Data.Algorithm.PPattern.Splitting
(
  partitionsIncreasings
)
where

  import qualified Data.List     as L
  import qualified Data.Foldable as F
  import qualified Data.Set      as Set

  import Data.Algorithm.PPattern.Types
  import qualified Data.Algorithm.PPattern.Isogram as Isogram
  import qualified Data.Algorithm.PPattern.IntPartition as IntPartition

  increasingSubsequences :: Isogram.Isogram ->  Length -> [Isogram.Isogram]
  increasingSubsequences (Isogram.Isogram xs) l = Isogram.fromList $ aux xs l z
    where
      aux :: IsogramL -> Length -> T -> [IsogramL]
      aux _      0 _  = [[]]
      aux []     _ _  = []
      aux (x:xs) l x'
        | x > x'    = L.map (x:) (aux xs (l-1) x) ++ aux xs l x'
        | otherwise = aux xs l x'

      z :: T
      z = (F.minimum xs)-1

  partitionsIncreasingsByLength :: IsogramL -> [Length] -> [[IsogramL]]
  partitionsIncreasingsByLength [] []     = [[]]
  partitionsIncreasingsByLength [] _      = []
  partitionsIncreasingsByLength _  []     = []
  partitionsIncreasingsByLength xs (l:ls) = ps
    where
      ps = [is:iss | is  <- increasingSubsequences xs l,
                     iss <- partitionsIncreasingsByLength (xs L.\\ is) ls]

  partitionsIncreasings :: IsogramL -> Int -> [[IsogramL]]
  partitionsIncreasings xs k = upToIsomorphism $ aux xs k
    where
      aux :: IsogramL -> Int -> [[IsogramL]]
      aux xs k = L.concat [partitionsIncreasingsByLength xs (IntPartition.toList p) |
                           p <- IntPartition.partitionsByLength (L.length xs) k]

      upToIsomorphism :: [[IsogramL]] -> [[IsogramL]]
      upToIsomorphism = Set.toList . Set.fromList . L.map L.sort
