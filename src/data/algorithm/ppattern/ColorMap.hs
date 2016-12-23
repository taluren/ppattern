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
, emptyXCoordColorMap
, emptyYCoordColorMap

  -- * Querying
, colors
, nbColors
, next

  -- * Updating
, updateForNext
)
where

  import qualified Data.List          as L
  import qualified Data.IntMap.Strict as IntMap

  import qualified Data.Algorithm.PPattern.Permutation    as Permutation
  import qualified Data.Algorithm.PPattern.Point          as Point
  import qualified Data.Algorithm.PPattern.PointMap       as PointMap
  import qualified Data.Algorithm.PPattern.Color          as Color
  import qualified Data.Algorithm.PPattern.CoordSelection as CoordSelection

  newtype ColorMap = ColorMap (IntMap.IntMap (PointMap.PointMap))
                     deriving (Show)

  {-|
    'colors m' return the colors (i.e. keys) used by the map.
  -}
  colors :: ColorMap -> [Color.Color]
  colors = IntMap.keys

  {-|
    'nbColors m' return the number of colors (i.e. keys) used by the map.
  -}
  nbColors :: ColorMap -> Int
  nbColors = L.length . colors

  {-|

  -}
  next :: Color.Color -> Point.Point -> ColorMap -> Maybe Point.Point
  next c p m = IntMap.lookup c m >>= PointMap.next p

  {-|

  -}
  updateForNext :: Color.Color -> Point.Point -> Permutation.T -> ColorMap -> Maybe ColorMap
  updateForNext c p x m =  Map.lookup c m >>= PointMap.updateForNext p x >>= aux
    where
      aux m' = Just (IntMap.update (\_ -> Just m') c m)
