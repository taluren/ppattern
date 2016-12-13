{-|
Module      : Data.Algorithm.PPattern.Rectangle
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Rectangle
(
  Rectangle(..)
  --
, makeRectangle
  --
, width
, height
  --
, enclosingRectangle
)
where

  import Data.Maybe

  import Data.Algorithm.PPattern.Point
  import Data.Algorithm.PPattern.Permutation

  data Rectangle a = Rectangle (Point a) (Point a)
                     deriving (Show, Eq)

  makeRectangle :: (Ord a) => Point a -> Point a -> Maybe (Rectangle a)
  makeRectangle p p'
    | p <= p'  = Just (Rectangle p p')
    | otherwise = Nothing

  makeRectangleUnsafe :: (Ord a) => Point a -> Point a -> Rectangle a
  makeRectangleUnsafe p p' = Rectangle p p'

  width :: (Ord a, Num a) => Rectangle a -> a
  width (Rectangle p1 p2) = x' - x
    where
      x  = xCoord p1
      x' = xCoord p2

  height :: (Ord a, Num a) => Rectangle a -> a
  height (Rectangle p1 p2) = y' - y
    where
      y  = yCoord p1
      y' = yCoord p2

  enclosingRectangle :: Ord a => Rectangle a -> Rectangle a -> Rectangle a
  enclosingRectangle r1 r2 = Rectangle ll' ur'
    where
      -- lower left and upper right points of rectangle r1
      ll1 = lowerLeft  r1
      ur1 = upperRight r1

      -- coordinates of lower left and upper right points of rectangle r1
      xll1 = xCoord lowerLeft1
      yll1 = yCoord lowerLeft1
      xur1 = xCoord upperRight1
      yur1 = yCoord upperRight1

      -- lower left and upper right points of rectangle r2
      ll2  = lowerLeft  r2
      ur2 = upperRight r2

      -- coordinates of lower left and upper right points of rectangle r2
      xll2  = xCoord lowerLeft2
      yll2  = yCoord lowerLeft2
      xur2 = xCoord upperRight2
      yur2 = yCoord upperRight2

      -- coordinates of lower left and upper right points of enclosing rectangle
      ll' = Point (min xll1 xll2) (min yll1 yll2)
      ur' = Point (max xur1 xur2) (max yur1 yur2)

  makeDegenerateRectangle :: (Point a) -> Rectangle a
  makeDegenerateRectangle p = Rectangle p p

  fromPermutation :: Permutation a -> [Rectangle a]
  fromPermutation = L.foldr makeDegenerateRectangle [] . L.zipWith Point [1..] . toList

  xInterval :: Rectangle a -> Interval a
  xInterval r = makeInterval l r
    where
      l = xCoord (lowerLeft r)
      r = xCoord (upperRight r)

  yInterval :: Rectangle a -> Interval a
  yInterval r = makeInterval l r
    where
      l = yCoord (lowerLeft r)
      r = yCoord (upperRight r)
