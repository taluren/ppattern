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
  partitionsInIncreasings
)
where

  import qualified Data.List     as L
  import qualified Data.Foldable as F
  import qualified Data.Set      as Set

  import Data.Algorithm.PPattern.Types
  import Data.Algorithm.PPattern.Combinatorics

  increasingSubsequences :: Isogram ->  Length -> [Isogram]
  increasingSubsequences xs l = aux xs l z
    where
      aux :: Isogram -> Length -> T -> [Isogram]
      aux _      0 _  = [[]]
      aux []     _ _  = []
      aux (x:xs) l x'
        | x > x'    = L.map (x:) (aux xs (l-1) x) ++ aux xs l x'
        | otherwise = aux xs l x'

      z :: T
      z = (F.minimum xs)-1

  partitionsIncreasingsByLength :: Isogram -> [Length] -> [[Isogram]]
  partitionsIncreasingsByLength [] []     = [[]]
  partitionsIncreasingsByLength [] _      = []
  partitionsIncreasingsByLength _  []     = []
  partitionsIncreasingsByLength xs (l:ls) = ps
    where
      ps = [is:iss | is  <- increasingSubsequences xs l,
                     iss <- partitionsIncreasingsByLength (xs L.\\ is) ls]

  partitionsInIncreasings :: Isogram -> Int -> [[Isogram]]
  partitionsInIncreasings xs k = upToIsomorphism $ aux xs k
    where
      aux :: Isogram -> Int -> [[Isogram]]
      aux xs k = L.concat [partitionsIncreasingsByLength xs p |
                           p <- partitionsByLength (L.length xs) k]

      upToIsomorphism :: [[Isogram]] -> [[Isogram]]
      upToIsomorphism = Set.toList . Set.fromList . L.map L.sort
