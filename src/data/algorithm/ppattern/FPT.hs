{-|
Module      : Data.Algorithm.PPattern.FPT
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.FPT
(
  search
)
where

  import Data.Algorithm.PPattern.Types

  search :: Permutation -> Permutation -> Bool
  search = id
