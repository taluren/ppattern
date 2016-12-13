{-|
Module      : Data.Algorithm.PPattern.Isogram
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Isogram
(
  Isogram
)
where

  import qualified Data.Algorithm.PPPattern.Permutation

  type Isogram a = Permutation a
