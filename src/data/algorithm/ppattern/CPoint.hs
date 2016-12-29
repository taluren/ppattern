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

  -- * Querying list of colored points
, colors
, nbColors
)
where

  import qualified Data.List as L
  import qualified Data.Set  as Set

  import qualified Data.Algorithm.PPattern.Types as T
  import qualified Data.Algorithm.PPattern.Color as Color

  data CPoint = CPoint { xCoord :: {-# UNPACK #-} !T.T -- x coordinate
                       , yCoord :: {-# UNPACK #-} !T.T  -- y coordinate
                       , color :: {-# UNPACK #-} !Color.Color
                       } deriving (Show, Eq, Ord)

  {-|
    'mkPoint'' makes a colored point from two integer coordinates and a color.
  -}
  mkCPoint :: T.T -> T.T -> Color.Color -> CPoint
  mkCPoint x y c = CPoint { xCoord=x, yCoord=y, color=c }


  {-|
    Return the list of distinct colors that occur in a list of colored points.
  -}
  colors :: [CPoint] -> [Color.Color]
  colors = Set.toList . Set.fromList . fmap color

  {-|
    Return the number of distincts colors in a list of colored points.
  -}
  nbColors :: [CPoint] -> Int
  nbColors = L.length . colors
