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
  CPoint(..)
, Color
  --
, mkCPoint
, mkCPoint'
  --
, sameC
, diffC
)
where

  import qualified Data.Algorithm.PPattern.Point as Point

  type Color = Int

  data CPoint = CPoint { point :: {-# UNPACK #-} !Point.Point
                       , color :: {-# UNPACK #-} !Color
                       } deriving (Show, Eq)

  {-|
    'mkCPoint' mks a point from a point and a color.
  -}
  mkCPoint :: Point.Point -> Color -> CPoint
  mkCPoint p c = CPoint {point=p, color=c}

  {-|
    'mkPoint'' mks a point from two integers and a color.
  -}
  mkCPoint' :: Int -> Int -> Color -> CPoint
  mkCPoint' x y c = CPoint {point=Point.mkPoint x y, color=c}

  {-|
    'sameC p1 p2' returns True if the two colored points 'p1' and 'p2' have the
    same color.
  -}
  sameC :: CPoint -> CPoint -> Bool
  sameC p1 p2 = color p1 == color p2

  {-|
    'diffC p1 p2' returns True if the two colored points 'p1' and 'p2' have
    different colors.
  -}
  diffC :: CPoint -> CPoint -> Bool
  diffC p1 p2 = not $ sameC p1 p2
