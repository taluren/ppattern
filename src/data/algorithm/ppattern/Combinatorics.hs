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
  choose
)
where

  import qualified Data.List as L
  import qualified Data.Set  as Set

  {-|
    The 'partitionsByLength' function returns all k-partitions of an integer.
  -}
  choose :: [a] -> Int -> [[a]]
  _      `choose` 0       = [[]]
  []     `choose` _       =  []
  (x:xs) `choose` k       =  (x:) `fmap` (xs `choose` (k-1)) ++ xs `choose` k
