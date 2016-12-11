{-|
Module      : Data.Algorithm.PPattern.Types
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Types
(
  T
, Length
, Index
, Isogram
, Permutation
)
where

  -- |Isogram and Permutation element type
  type T = Int

  -- |'T' sequence type with no repeater letter
  type Isogram = [T]

  -- | 'T' permutation type
  type Permutation = [T]

  -- |Integer length type
  type Length = Int

  -- |Integer index type
  type Index = Int
