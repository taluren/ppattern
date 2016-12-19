{-|
Module      : Data.Algorithm.PPattern.Permutation.Stringology
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Permutation.Stringology
(
  longestIncreasingSub
, lenLongestIncreasingSub
  --
, longestDecreasingSub
, lenLongestDecreasingSub
)
where

  import qualified Data.List               as L
  import qualified Data.Tuple              as T
  import qualified Data.Map                as Map
  import qualified Data.Algorithm.Patience as Patience

  import qualified Data.Algorithm.PPattern.Permutation as Permutation

  {-|
    'longestIncreasingSub xs' returns a longest increasing subsequences in 'xs'.
  -}
  longestIncreasingSub :: Permutation.Permutation -> Permutation.Permutation
  longestIncreasingSub = post . Patience.longestIncreasing . pre
    where
      pre  = flip L.zip [1..] . Permutation.toList
      post =  Permutation.fromListUnsafe . L.map T.fst . L.reverse

  {-|
    'lenLongestIncreasingSub xs' returns the length of the longest increasing
    subsequences in 'xs'.
  -}
  lenLongestIncreasingSub:: Permutation.Permutation -> Int
  lenLongestIncreasingSub = L.length . longestIncreasingSub

  {-|
    'longestDecreasingSub xs' returns a longest decreasing subsequences in 'xs'.
  -}
  longestDecreasingSub :: Permutation.Permutation -> Permutation.Permutation
  longestDecreasingSub = post . Patience.longestIncreasing . pre
    where
      pre  = flip L.zip [1..] . L.reverse . Permutation.toList
      post = Permutation.fromListUnsafe . L.reverse . L.map T.fst . L.reverse

  {-|
    'lenLongestDecreasingSub xs' returns the length of the longest decreasing
    subsequences in 'xs'.
  -}
  lenLongestDecreasingSub :: Permutation.Permutation -> Int
  lenLongestDecreasingSub = L.length . longestDecreasingSub
