{-|
Module      : Data.Algorithm.PPattern.LCS
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.LCS
(
  Data.Algorithm.PPattern.LCS.longestIncreasing
, longestIncreasingLength
)
where

  import qualified Data.List  as L
  import qualified Data.Tuple as T
  import Data.Algorithm.Patience as Patience

  {-|
    The 'square' function squares an integer.
    It takes one argument, of type 'Int'.
  -}
  longestIncreasing :: [Int] -> [Int]
  longestIncreasing = post . Patience.longestIncreasing . pre
    where
      pre  = flip L.zip [1..]
      post = L.map T.fst . L.reverse

  {-|
    The 'square' function squares an integer.
    It takes one argument, of type 'Int'.
  -}
  longestIncreasingLength :: [Int] -> Int
  longestIncreasingLength = L.length . Data.Algorithm.PPattern.LCS.longestIncreasing
