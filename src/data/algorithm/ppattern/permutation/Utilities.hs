{-|
Module      : Data.Algorithm.PPattern.Permutation.Utilities
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Permutation.Utilities
(
  increasing
, decreasing
, empty
  --
, (\\)
)
where

  import qualified Data.List as L

  import Data.Algorithm.PPattern.Permutation.Core

  {-|
    'increasing n' return the increasing permutation 1 2 ... n. The function
    returns the empty permutation if 'n' is non-positive.
  -}
  increasing :: Int -> Permutation
  increasing n = fromListUnsafe [1..n]

  {-|
    'decrasing n' return the decreasing permutation n n-1 ... 1. The function
    returns the empty permutation if 'n' is non-positive.
  -}
  decreasing :: Int -> Permutation
  decreasing n = fromListUnsafe [n,n-1..1]

  {-|
    'empty' returns the empty permutation (i.e., the permutation with no element).
  -}
  empty :: Permutation
  empty = fromListUnsafe []


  {-|
  -}
  (\\) :: Permutation -> Permutation -> Permutation
  (\\) (Permutation xs) (Permutation ys) = fromListUnsafe $ xs L.\\ ys
