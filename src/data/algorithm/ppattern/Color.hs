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

--   -- * Enumerating
-- , colors
--
--   -- * Transforming
-- , toInt
-- , fromInt
)
where

  --import qualified Data.Word as Word

  -- |The 'Color' type encapsulates a color.
   type Color = Int

  -- -- |'colors n' return a list of 'n' distinct colors.
  -- colors :: Int -> [Color]
  -- colors n = fmap fromInt [1..n]

  -- -- |Transform a 'Color' into an int.
  -- toInt :: Color -> Int
  -- toInt c = fromIntegral c :: Int
  --
  -- -- |Tranform an int into a 'Color'.
  -- fromInt :: Int -> Color
  -- fromInt c = fromIntegral c :: Color
