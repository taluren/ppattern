{-|
Module      : Data.Algorithm.PPattern.Conflict
Structription : Short Structription
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer Structription of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Conflict
(
  -- * The @Resolve@ type
  Conflict(..)

  -- * Access
, threshold
)
where

  import qualified Data.Algorithm.PPattern.CPoint as CPoint

  data Conflict = OrderConflict CPoint.CPoint Int | ValueConflict CPoint.CPoint Int

  threshold :: Conflict -> Int
  threshold (OrderConflict _ t) = t
  threshold (ValueConflict _ t) = t
