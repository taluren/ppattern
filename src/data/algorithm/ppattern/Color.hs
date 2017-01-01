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

  -- * Transforming
, toInt
, fromInt
)
where

  import qualified Data.Word as Word

  type Color = Word.Word8

  toInt :: Color -> Int
  toInt c = fromIntegral c :: Int

  fromInt :: Int -> Color
  fromInt c = fromIntegral c :: Color
