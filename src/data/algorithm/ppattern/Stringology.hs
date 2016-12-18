{-|
Module      : Data.Algorithm.PPattern.Stringology
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Stringology
(
  longestIncreasingSub
, lenLongestIncreasingSub
  --
, longestDecreasingSub
, lenLongestDecreasingSub
  --
, nextIncreasing
)
where

  import qualified Data.List  as L
  import qualified Data.Tuple as T
  import qualified Data.Map   as Map
  import Data.Algorithm.Patience as Patience

  {-|
    'longestIncreasingSub xs' returns a longest increasing subsequences in 'xs'.
  -}
  longestIncreasingSub :: [Int] -> [Int]
  longestIncreasingSub = post . Patience.longestIncreasing . pre
    where
      pre  = flip L.zip [1..]
      post = L.map T.fst . L.reverse

  {-|
    'lenLongestIncreasingSub xs' returns the length of the longest increasing
    subsequences in 'xs'.
  -}
  lenLongestIncreasingSub:: [Int] -> Int
  lenLongestIncreasingSub = L.length . longestIncreasingSub

  {-|
    'longestDecreasingSub xs' returns a longest decreasing subsequences in 'xs'.
  -}
  longestDecreasingSub :: [Int] -> [Int]
  longestDecreasingSub = post . Patience.longestIncreasing . pre
    where
      pre  = flip L.zip [1..] . L.reverse
      post = L.reverse . L.map T.fst . L.reverse

  {-|
    'lenLongestDecreasingSub xs' returns the length of the longest decreasing
    subsequences in 'xs'.
  -}
  lenLongestDecreasingSub :: [Int] -> Int
  lenLongestDecreasingSub = L.length . longestDecreasingSub

  {-|
    'nextIncreasing'
  -}
  nextIncreasing :: [Int] -> Map.Map (Int, Int) (Int, Int)
  nextIncreasing [] = Map.empty
  nextIncreasing xs = aux (L.zip [1..] xs) [] Map.empty
    where
      aux ((i,x):ixs) s m = nextIncreasingAux ixs [(i,x)] m

  -- nextIncreasingAux :: [(Int, Int)] -> [(Int, Int)] -> Map.Map (Int, Int) (Int, Int) -> Map.Map (Int, Int) (Int, Int)
  -- nextIncreasingAux []          s           m = m
  -- nextIncreasingAux ((i,x):ixs) []          m = nextIncreasingAux ixs [(i,x)] m
  -- nextIncreasingAux ixs'@((i,x):ixs) jys'@((j,y):jys) m
  --   | y < x     = aux ixs' jys' m
  --   | otherwise = nextIncreasingAux ixs ((i,x):jys') m
  --   where
  --     aux ixs         []          m = nextIncreasingAux ixs [] m
  --     aux ((i,x):ixs) jys'@((j,y):jys) m
  --       | y < x     = aux ((i,x):ixs) jys (Map.insert (j,y) (i,x) m)
  --       | otherwise = nextIncreasingAux ixs ((i,x):jys') m

  nextIncreasingAux :: [(Int, Int)] -> [(Int, Int)] -> Map.Map (Int, Int) (Int, Int) -> Map.Map (Int, Int) (Int, Int)
  nextIncreasingAux []          s           m = m
  nextIncreasingAux ((i,x):ixs) []          m = nextIncreasingAux ixs [(i,x)] m
  nextIncreasingAux ixs'@((i,x):ixs) jys'@((j,y):jys) m = aux ixs' jys' m
    where
      aux ixs         []          m = nextIncreasingAux ixs [] m
      aux ((i,x):ixs) jys'@((j,y):jys) m
        | y < x     = aux ((i,x):ixs) jys (Map.insert (j,y) (i,x) m)
        | otherwise = nextIncreasingAux ixs ((i,x):jys') m
