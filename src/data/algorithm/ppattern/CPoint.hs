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

module Data.Algorithm.PPattern.Point
(
  CPoint(..)
, C
  --
, mkCPoint
, mkCPoint'
  --
, sameC
)
where

  type C = Int

  data CPoint = CPoint { point :: Point
                       , color :: C
                       } deriving (Show, Eq)

  {-|
    'mkCPoint' mks a point from a point and a color.
  -}
  mkCPoint :: Point -> Color -> CPoint
  mkPoint p c = CPoint {point=p, color=c}

  {-|
    'mkPoint'' mks a point from two integers and a color.
  -}
  mkCPoint' :: Int -> Int -> C -> CPoint
  mkCPoint' x y c = Point {point=mkPoint x y, color=c}

  {-|
    'sameC p1 p2' returns True if the two colored points 'p1' and 'p2' have the
    same color.
  -}
  sameC :: CPoint -> CPoint -> Bool
  sameC p1 p2 = color p1 == color p2
