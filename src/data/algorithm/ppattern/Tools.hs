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
  sortPairFst
, sortPairSnd
)
where

  import qualified Data.List  as L
  import qualified Data.Tuple as T

  sortPairFst :: Ord a => [(a, b)] -> [(a, b)]
  sortPairFst = L.sortBy comp
    where
      comp t1 t2 = (T.fst t1) `compare` (T.fst t2)

  sortPairSnd :: Ord b => [(a, b)] -> [(a, b)]
  sortPairSnd = L.sortBy comp
    where
      comp t1 t2 = (T.snd t1) `compare` (T.snd t2)