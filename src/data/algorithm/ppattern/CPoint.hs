{-|
Module      : Data.Algorithm.PPattern.CPoint
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.CPoint
(
  -- * The @CPoint@ type
  CPoint

 -- * Accessing
, xCoord
, yCoord
, color

  -- * constructing
, mkCPoint

  -- * Modifying
, updateXCoord
, updateYCoord
, updateColor

  -- * Displaying
, toStr
)
where

  import qualified Data.Monoid as Monoid

  import qualified Data.Algorithm.PPattern.Color as Color

  type Point = (Int, Int)
  type CPoint = (Point, Color.Color)

  {-|
    'mkPoint' makes a colored CPoint from two integer coordinates and a color.
  -}
  mkCPoint :: Int -> Int -> Color.Color -> CPoint
  mkCPoint x y c = ((x, y), c)

  {-|
    Get x-ccordinate.
  -}
  xCoord :: CPoint -> Int
  xCoord ((x, _), _) = x

  {-|
    Get y-ccordinate.
  -}
  yCoord :: CPoint -> Int
  yCoord ((_, y), _) = y

  {-|
    Get color.
  -}
  color :: CPoint -> Color.Color
  color (_, c) = c

  {-|
    Update x-ccordinate.
  -}
  updateXCoord :: Int -> CPoint -> CPoint
  updateXCoord x' ((_, y), c) = ((x', y), c)

  {-|
    Update y-ccordinate.
  -}
  updateYCoord :: Int -> CPoint -> CPoint
  updateYCoord y' ((x, _), c) = ((x, y'), c)

  {-|
    Update color.
  -}
  updateColor :: Color.Color -> CPoint -> CPoint
  updateColor c' (p, _) = (p, c')

  {-|
    Transform to string.
  -}
  toStr :: CPoint -> String
  toStr ((x, y), c) =
      "("    `Monoid.mappend`
      "x="   `Monoid.mappend`
      show x `Monoid.mappend`
      ","    `Monoid.mappend`
      "y="   `Monoid.mappend`
      show y `Monoid.mappend`
      ","    `Monoid.mappend`
      "c="   `Monoid.mappend`
      show c `Monoid.mappend`
      ")"
