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
)
where

  import qualified Data.Tuple as T

  type Point a = (a,a)

  {-|
    'makePoint' makes a point from two integers 'x' and 'y'.
  -}
  makePoint :: a -> a -> Point a
  makePoint x y = (x, y)

  {-|
    'xCoord' returns the x-coordinate of a point.
  -}
  xCoord :: Point a -> a
  xCoord = T.fst

  {-|
    'yCoord' returns the y-coordinate of a point.
  -}
  yCoord :: Point a -> a
  yCoord = T.snd

  p1 `southEastDomination` p2 = xConstraint && yConstraint
    where
      xConstraint = xCoord p1 < xCoord p2
      yConstraint = yCoord p1 > yCoord p2
