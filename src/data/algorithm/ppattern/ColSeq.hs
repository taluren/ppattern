{-|
Module      : Data.Algorithm.PPattern.ColMapping
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.ColMapping
(

)
where

  import qualified Data.List     as L
  import qualified Data.Foldable as F
  import qualified Data.Tuple    as T
  import qualified Data.Set      as Set

  import Data.Algorithm.PPattern.ColPoint

  newtype ColSeq a c = ColSeq [(a, c)]
