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

  -- * Querying list of colored CPoints
, colors
, nbColors

  -- * Comparing
, northWestDomination
, northEastDomination
, southWestDomination
, southEastDomination
)
where

  import qualified Data.List as L
  import qualified Data.Set  as Set

  import qualified Data.Algorithm.PPattern.Types as T
  import qualified Data.Algorithm.PPattern.Color as Color

  data CPoint = CPoint { xCoord :: {-# UNPACK #-} !T.T
                       , yCoord :: {-# UNPACK #-} !T.T
                       , color  :: {-# UNPACK #-} !Color.Color
                       } deriving (Show, Eq, Ord)

  {-|
    'mkPoint'' makes a colored CPoint from two integer coordinates and a color.
  -}
  mkCPoint :: T.T -> T.T -> Color.Color -> CPoint
  mkCPoint x y c = CPoint { xCoord=x, yCoord=y, color=c }


  {-|
    Return the list of distinct colors that occur in a list of colored CPoints.
  -}
  colors :: [CPoint] -> [Color.Color]
  colors = Set.toList . Set.fromList . fmap color

  {-|
    Return the number of distincts colors in a list of colored CPoints.
  -}
  nbColors :: [CPoint] -> Int
  nbColors = L.length . colors

  {-|
    'southWestDomination p1 p2' takes two CPoints. It returns True if and only if
    'p1' south-west-dominates 'p2' (i.e., x1 > x2 and y1 < y2).
  -}
  northWestDomination :: CPoint -> CPoint -> Bool
  northWestDomination = flip southEastDomination

  {-|
    'southEastDomination p1 p2' takes two CPoints. It returns True if and only if
    'p1' south-east-dominates 'p2' (i.e., x1 < x2 and y1 > y2).
  -}
  northEastDomination :: CPoint -> CPoint -> Bool
  p1 `northEastDomination` p2 = xConstraint && yConstraint
    where
      xConstraint = xCoord p1 < xCoord p2
      yConstraint = yCoord p1 < yCoord p2

  {-|
    'southWestDomination p1 p2' takes two CPoints. It returns True if and only if
    'p1' south-west-dominates 'p2' (i.e., x1 > x2 and y1 > y2).
  -}
  southWestDomination :: CPoint -> CPoint -> Bool
  southWestDomination = flip northEastDomination

  {-|
    'southEastDomination p1 p2' takes two CPoints. It returns True if and only if
    'p1' north-east-dominates 'p2' (i.e., x1 < x2 and y1 > y2).
  -}
  southEastDomination :: CPoint -> CPoint -> Bool
  p1 `southEastDomination` p2 = xConstraint && yConstraint
    where
      xConstraint = xCoord p1 < xCoord p2
      yConstraint = yCoord p1 > yCoord p2
