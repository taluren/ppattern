{-|
Module      : Data.Algorithm.PPattern.Permutation.Internal
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Permutation.Internal
(
  Permutation(..)
)
where

  type TPermutation = Int

  newtype Permutation = Permutation [TPermutation] deriving (Show, Eq, Ord)
