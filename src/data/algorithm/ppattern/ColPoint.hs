{-|
Module      : Data.Algorithm.PPattern.ColPoint
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
  ColPoint(..)
  --
, makePoint
  --
, xCoord
, yCoord
  --
)
where

  data ColPoint a c = Point { point :: Point a
                            , col   :: b
                            } deriving (Show, Eq)

  {-|
    'makePoint' makes a point from two integers 'x' and 'y'.
  -}
  makeColPoint :: Point a -> c -> Point a c
  makePoint p c = Point {point=p, col=c}
