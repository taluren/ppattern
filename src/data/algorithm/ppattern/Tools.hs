{-|
Module      : Data.Algorithm.PPattern.Tools
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Tools
(
  Permutation
)
where

  sortPairFst :: Ord a => [(a, b)] -> [(a, b)]
  sortPairFst = L.sortBy comp
    where
      comp t t' = (T.fst t1) `compare` (T.fst t2)

  sortPairSnd :: Ord b => [(a, b)] -> [(a, b)]
  sortPairSnd = L.sortBy comp
    where
      comp t t' = (T.snd t1) `compare` (T.snd t2)
