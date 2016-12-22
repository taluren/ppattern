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
  -- * The @ColorMap@ type
  ColorMap

  -- * Querying
, colors
, nbColors
, next

  -- * Updating
, updateForNext
)
where

  import qualified Data.List       as L
  import qualified Data.Map.Strict as Map

  import qualified Data.Algorithm.PPattern.Permutation as Permutation
  import qualified Data.Algorithm.PPattern.Point       as Point
  import qualified Data.Algorithm.PPattern.PointMap    as PointMap
  import qualified Data.Algorithm.PPattern.Color       as Color

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
  next :: Color.Color -> Point.Point -> ColorMap -> Maybe Point.Point
  next c p m = Map.lookup c m >>= PointMap.next p

  {-|

  -}
  updateForNext :: Color.Color -> Point.Point -> Permutation.T -> ColorMap -> Maybe ColorMap
  updateForNext c p x m =  Map.lookup c m >>= PointMap.updateForNext p x >>= aux
    where
      aux m' = Just (Map.update (\_ -> Just m') c m)
