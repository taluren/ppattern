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

module Data.Algorithm.PPattern.CPoint
(
  -- * The @CPoint@ type
  CPoint(..)
, mkCPoint
, mkCPoint'

  -- * Querying
, colors
, nbColors
, xCoord
, yCoord
)
where

  import qualified Data.List as L

  import qualified Data.Algorithm.PPattern.Types as T
  import qualified Data.Algorithm.PPattern.Point as Point
  import qualified Data.Algorithm.PPattern.Color as Color

  data CPoint = CPoint { point :: {-# UNPACK #-} !Point.Point
                       , color :: {-# UNPACK #-} !Color.Color
                       } deriving (Show, Eq, Ord)

  {-|
    'mkCPoint' makes a colored point from a point and a color.
  -}
  mkCPoint :: Point.Point -> Color.Color -> CPoint
  mkCPoint p c = CPoint {point=p, color=c}

  {-|
    'mkPoint'' makes a colored point from two integer coordinates and a color.
  -}
  mkCPoint' :: T.T -> T.T -> Color.Color -> CPoint
  mkCPoint' x y = mkCPoint p
    where
      p = Point.mkPoint x y

  {-|
    Return the x-coordinate of the colored point.
  -}
  xCoord :: CPoint -> T.T
  xCoord = Point.xCoord . point

  {-|
    Return the y-coordinate of the colored point.
  -}
  yCoord :: CPoint -> T.T
  yCoord = Point.yCoord . point

  {-|
    Return the list of distinct colors that occur in a list of colored points.
  -}
  colors :: [CPoint] -> [Color.Color]
  colors = L.nub . L.map color

  {-|
    Return the number of distincts colors in a list of colored points.
  -}
  nbColors :: [CPoint] -> Int
  nbColors = L.length . colors
