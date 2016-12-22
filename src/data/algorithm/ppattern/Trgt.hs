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
  -- * The @Trgt@ type
  Trgt(..)
, mkTrgt

-- * Accessors
, nextMaps
, cPoints

-- * Querying
, colors
, nbColors
)
where

  import qualified Data.List       as L
  import qualified Data.Map.Strict as Map

  import qualified Data.Algorithm.PPattern.Permutation as Permutation
  import qualified Data.Algorithm.PPattern.CPoint      as CPoint
  import qualified Data.Algorithm.PPattern.Point       as Point
  import qualified Data.Algorithm.PPattern.Color       as Color
  import qualified Data.Algorithm.PPattern.PointMap    as PointMap
  import qualified Data.Algorithm.PPattern.ColorMap    as ColorMap

  -- | Trgt data
  newtype Trgt = Trgt ([CPoint.CPoint],  ColorMap.ColorMap)
                 deriving (Show)

  {-|
    'mkTrgt' makes a target from a list of colored points 'cps' and a next
    mapping.
  -}
  mkTrgt :: [CPoint.CPoint] -> ColorMap.ColorMap -> Trgt
  mkTrgt cps m = Struct (cps, m)

  {-|
    The 'cPoints' function returns the colored points stored in 'trgt'.
  -}
  cPoints :: Trgt -> [CPoint.CPoint]
  cPoints Trgt (cPoints, _) = cPoints

  {-|
    The 'nextMap' function returns the next map.
  -}
  nextMaps :: Trgt -> ColorMap.ColorMap
  nextMaps Trgt (_, m) = m

  {-|
    The 'colors' function returns the list of all colors in the target.
  -}
  colors :: Trgt -> [Color.Color]
  colors = Map.keys . nextMaps

  {-|
    The 'nbColors' function returns the number of colors in the target.
  -}
  nbColors :: Trgt -> [Color.Color]
  nbColors = L.length . colors
