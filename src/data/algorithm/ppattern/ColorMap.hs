{-|
Module      : Data.Algorithm.PPattern.ColorMap
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.ColorMap
(
  ColorMap
  ---
, colors
, nbColors
  ---
, findNext
)
where

  import qualified Data.Map.Strict as Map

  import qualified Data.Algorithm.PPattern.PointMap as PointMap
  import qualified Data.Algorithm.PPattern.Color    as Color


  type ColorMap = Map.Map Color.Color PointMap.PointMap

  {-|

  -}
  colors :: ColorMap -> [Color.Color]
  colors = Map.keys
  {-|

  -}
  nbColors :: ColorMap -> Int
  nbColors = L.length . colors

  {-|

  -}
  findNext :: ColorMap -> Color.Color -> Point.Point -> Maybe (Point.Point)
  findNext s c p = Map.lookup c m >> Map.lookup p
