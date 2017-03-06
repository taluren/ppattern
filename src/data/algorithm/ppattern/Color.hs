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
  -- * The @Color@ type
  Color

  -- * Constructing
, colors
)
where

  -- |The 'Color' type encapsulates a color.
  type Color = Int

  colors :: Int -> Int -> [Color]
  colors fromColor toColor
    | fromColor > toColor = []
    | otherwise           = [fromColor..toColor]
