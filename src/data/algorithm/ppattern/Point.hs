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
, mkPoint
  --
, xCoord
, yCoord
  --
, northWestDomination
, northEastDomination
, southWestDomination
, southEastDomination
)
where

  data Point = Point {-# UNPACK #-} !Int
                     {-# UNPACK #-} !Int
                     deriving (Show, Eq, Ord)

  {-|
    'mkPoint' mks a point from two integers 'x' and 'y'.
  -}
  mkPoint :: Int -> Int -> Point
  mkPoint = Point

  {-|
    'xCoord' returns the x-coordinate of a point.
  -}
  xCoord :: Point -> Int
  xCoord (Point x _) = x

  {-|
    'yCoord' returns the y-coordinate of a point.
  -}
  yCoord :: Point -> Int
  yCoord (Point _ y) = y

  {-|
    'southWestDomination p1 p2' takes two points. It returns True if and only if
    'p1' south-west-dominates 'p2' (i.e., x1 > x2 and y1 < y2).
  -}
  northWestDomination :: Point -> Point -> Bool
  northWestDomination = flip southEastDomination

  {-|
    'southEastDomination p1 p2' takes two points. It returns True if and only if
    'p1' south-east-dominates 'p2' (i.e., x1 < x2 and y1 > y2).
  -}
  northEastDomination :: Point -> Point -> Bool
  p1 `northEastDomination` p2 = xConstraint && yConstraint
    where
      xConstraint = xCoord p1 < xCoord p2
      yConstraint = yCoord p1 < yCoord p2

  {-|
    'southWestDomination p1 p2' takes two points. It returns True if and only if
    'p1' south-west-dominates 'p2' (i.e., x1 > x2 and y1 > y2).
  -}
  southWestDomination :: Point -> Point -> Bool
  southWestDomination = flip northEastDomination

  {-|
    'southEastDomination p1 p2' takes two points. It returns True if and only if
    'p1' north-east-dominates 'p2' (i.e., x1 < x2 and y1 > y2).
  -}
  southEastDomination :: Point -> Point -> Bool
  p1 `southEastDomination` p2 = xConstraint && yConstraint
    where
      xConstraint = xCoord p1 < xCoord p2
      yConstraint = yCoord p1 > yCoord p2
