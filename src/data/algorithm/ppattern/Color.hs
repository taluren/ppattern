{-|
Module      : Data.Algorithm.PPattern.Color
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Color
(
  Color(..)
  ---
, mkColor
)
where

  data Color = Color {-# UNPACK #-} !Int
               deriving (Show, Eq, Ord)


  {-|
   'mkColor' makes a color from an integer.
  -}
  mkColor :: Int -> Color
  mkColor = Color
