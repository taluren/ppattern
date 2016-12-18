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
  --
, makeCPoint
, makeCPoint'
  --
, sameC
)
where

  data CPoint c = CPoint { point :: Point
                         , color :: c
                         } deriving (Show, Eq)

  {-|
    'makeCPoint' makes a point from a point and a color.
  -}
  makeCPoint :: Point -> c -> CPoint c
  makePoint p c = CPoint {point=p, color=c}

  {-|
    'makePoint'' makes a point from two integers and a color.
  -}
  makeCPoint' :: Int -> Int -> c -> CPoint c
  makeCPoint' x y c = Point {point=makePoint x y, color=c}

  {-|
    'sameC p1 p2' returns True if the two colored points 'p1' and 'p2' have the
    same color.
  -}
  sameC :: (Eq c) => CPoint c -> CPoint c -> Bool
  sameC p1 p2 = color p1 == color p2
