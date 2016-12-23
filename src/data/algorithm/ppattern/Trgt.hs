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

  import qualified Data.Algorithm.PPattern.CPoint      as CPoint
  import qualified Data.Algorithm.PPattern.Color       as Color
  import qualified Data.Algorithm.PPattern.ColorMap    as ColorMap

  -- | Trgt data
  newtype Trgt = Trgt ([CPoint.CPoint], ColorMap.ColorMap)
                 deriving (Show)

   -- | Trgt data
   data Trgt = Trgt { cPoints   :: [CPoint.CPoint]
                    , xColorMap :: ColorMap.ColorMap
                    , yColorMap :: ColorMap.ColorMap
                    } deriving (Show)

  {-|
    'mkTrgt' makes a target from a list of colored points 'cps', the x-coordinate
    next mapping and the y-coordinate next mapping.
  -}
  mkTrgt :: [CPoint.CPoint] -> ColorMap.ColorMap -> ColorMap.ColorMap -> Trgt
  mkTrgt cps xcm ycm = Trgt {cPoints=cps, xColorMap:xcm, yColorMap=ycm}

  {-|
    The 'colors' function returns the list of all colors in the target.
  -}
  colors :: Trgt -> [Color.Color]
  colors = Map.keys . xColorMap

  {-|
    The 'nbColors' function returns the number of colors in the target.
  -}
  nbColors :: Trgt -> Int
  nbColors = L.length . colors
