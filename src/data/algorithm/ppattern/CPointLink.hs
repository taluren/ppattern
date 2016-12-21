{-|
Module      : Data.Algorithm.PPattern.CPointLink
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.CPointLink
(
  CPointLink(..)
  --
, mkCPointLink
  --
, color
  --
, monoChromatic
, biChromatic
, orderConflict
, valueConflict
, biChromaticOrderConflict
, biChromaticValueConflict
)
where

  import qualified Data.Algorithm.PPattern.Point  as Point
  import qualified Data.Algorithm.PPattern.CPoint as CPoint
  import qualified Data.Algorithm.PPattern.Color  as Color

  data CPointLink = CPointLink { src  :: {-# UNPACK #-} !CPoint.CPoint
                                           , trgt :: {-# UNPACK #-} !CPoint.CPoint
                                           } deriving (Show, Eq)
  {-|
    'mkCPointLink' makes a CPointLink object from two colored points with the
    the same color.
  -}
  mkCPointLink :: CPoint.CPoint -> CPoint.CPoint -> Maybe CPointLink
  mkCPointLink src trgt
    | CPoint.diffC src trgt = Nothing
    | otherwise             = Just (CPointLink src trgt)

  {-|
    'color' return the color of a CPointLink.
  -}
  color :: CPointLink -> Color.Color
  color = CPoint.color . src

  {-|
    'sameColor' takes two CPointLink objects. It returns True if and only if the
    two links have the same color.
  -}
  monoChromatic :: CPointLink -> CPointLink -> Bool
  monoChromatic cpl1 cpl2 = color cpl1 == color cpl2

  {-|

  -}
  biChromatic :: CPointLink -> CPointLink -> Bool
  biChromatic cpl1 cpl2 = not $ monoChromatic cpl1 cpl2

  {-|

  -}
  orderConflict :: CPointLink -> CPointLink -> Bool
  orderConflict l1 l2 = (x1 < x2 && x1' > x2') || (x1 > x2 && x1' < x2')
    where
      x1  = Point.xCoord . CPoint.point $ src  l1
      x1' = Point.xCoord . CPoint.point $ trgt l1

      x2  = Point.xCoord . CPoint.point $ src  l2
      x2' = Point.xCoord . CPoint.point $ trgt l2

  {-|
  -}
  biChromaticOrderConflict :: CPointLink -> CPointLink -> Bool
  biChromaticOrderConflict l1 l2 = biChromatic l1 l2 && orderConflict l1 l2

  {-|

  -}
  valueConflict :: CPointLink -> CPointLink -> Bool
  valueConflict l1 l2 = (y1 < y2 && y1' > y2') || (y1 > y2 && y1' < y2')
    where
      y1  = Point.yCoord . CPoint.point $ src  l1
      y1' = Point.yCoord . CPoint.point $ trgt l1

      y2  = Point.yCoord . CPoint.point $ src  l2
      y2' = Point.yCoord . CPoint.point $ trgt l2

  biChromaticValueConflict :: CPointLink -> CPointLink -> Bool
  biChromaticValueConflict l1 l2 = biChromatic l1 l2 && valueConflict l1 l2
