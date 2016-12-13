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

  data Point a = Point a a deriving (Show, Eq)

  instance (Eq a, Ord a) => Ord (Point a) where
    (Point x y) <= (Point x' y') = x <= x' && y <= y'

  makePoint :: a -> a -> Point a
  makePoint x y = Point x y

  xCoord :: Point a -> a
  xCoord (Point x _) = x

  yCoord :: Point a -> a
  yCoord (Point _ y) = y
