{-|
Module      : Data.Algorithm.PPattern.Trgt
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Trgt
(
  Trgt(..)
  --
, mkTrgt
  ---
, colors
, nbColors
  ---
, nextMap
, nextByColorMap
)
where

  import qualified Data.List       as L
  import qualified Data.Map.Strict as Map

  import qualified Data.Algorithm.PPattern.Permutation as Permutation
  import qualified Data.Algorithm.PPattern.CPoint      as CPoint
  import qualified Data.Algorithm.PPattern.Color       as Color
  import qualified Data.Algorithm.PPattern.PointMap    as PointMap
  import qualified Data.Algorithm.PPattern.ColorMap    as ColorMap

  newtype Trgt = Trgt ([CPoint.CPoint],  ColorMap.ColorMap)
                 deriving (Show)

  {-|
    'mkPoint' mks a point from two integers 'x' and 'y'.
  -}
  mkTrgt :: [CPoint.CPoint] -> ColorMap.ColorMap -> Trgt
  mkTrgt cps m = Struct (cps, m)

  cPoints :: Trgt -> [CPoint.CPoint]
  cPoints Trgt (cPoints, _) = cPoints

  nextMap :: Trgt -> ColorMap.ColorMap
  nextMap Trgt (_, next) = next

  nextByColorMap :: Trgt -> Color.Color -> Maybe (PointMap.PointMap)
  nextByColorMap trgt c = Map.lookup c (nextMap trgt)

  next :: Trgt -> Color.Color -> Permutation.T -> Maybe (Point.Point)
  next trgt c x = nextByColorMap trgt c >>= PointMap.next x
