{-|
Module      : Data.Algorithm.PPattern.Point
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
  Point(..)
  --
, makePoint
  --
, xCoord
, yCoord
  --
, southEastDomination
)
where

  import qualified Data.Tuple as T

  newtype Point = Point (Int, Int)

  {-|
    'makePoint' makes a point from two integers 'x' and 'y'.
  -}
  makePoint :: Int -> Int -> Point
  makePoint x y = Point (x, y)

  {-|
    'xCoord' returns the x-coordinate of a point.
  -}
  xCoord :: Point -> Int
  xCoord (Point (x,_)) = x

  {-|
    'yCoord' returns the y-coordinate of a point.
  -}
  yCoord :: Point -> Int
  yCoord (Point (_,y)) = y

  {-|
    'southEastDomination p1 p2' takes two points. It returns True if and only if
    'p1' south-east-dominates 'p2' (i.e., x1 < x2 and y1 > y2).
  -}
  southEastDomination :: Point -> Point -> Bool
  p1 `southEastDomination` p2 = xConstraint && yConstraint
    where
      xConstraint = xCoord p1 < xCoord p2
      yConstraint = yCoord p1 > yCoord p2
