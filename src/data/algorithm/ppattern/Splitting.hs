module Data.Algorithm.PPattern.Splitting
(
  partitionsInIncreasings
)
where

import qualified Data.List     as L
import qualified Data.Foldable as F
import qualified Data.Set      as Set

import Types
import Combinatorics

increasingSubsequences :: Isogram ->  Length -> [Isogram]
increasingSubsequences xs l = aux xs l 0
  where
    aux :: Isogram -> Length -> T -> [Isogram]
    aux _      0 _  = [[]]
    aux []     _ _  = []
    aux (x:xs) l x'
      | x > x'    = L.map (x:) (aux xs (l-1) x) ++ aux xs l x'
      | otherwise = aux xs l x'

partitionsInIncreasingsByLength :: Isogram -> [Length] -> [[Isogram]]
partitionsInIncreasingsByLength [] []     = [[]]
partitionsInIncreasingsByLength [] _      = []
partitionsInIncreasingsByLength _  []     = []
partitionsInIncreasingsByLength xs (l:ls) = ps
  where
    ps = [
           is:iss |
           is  <- increasingSubsequences xs l,
           iss <- partitionsInIncreasingsByLength (xs L.\\ is) ls
         ]

partitionsInIncreasings :: Isogram -> Int -> [[Isogram]]
partitionsInIncreasings xs k = upToIsomorphism $ aux xs k
  where
    aux :: Isogram -> Int -> [[Isogram]]
    aux xs k = L.concat [
                         partitionsInIncreasingsByLength xs p |
                         p <- partitionsByLength (L.length xs) k
                        ]

    upToIsomorphism :: [[Isogram]] -> [[Isogram]]
    upToIsomorphism = Set.toList . Set.fromList . L.map L.sort
