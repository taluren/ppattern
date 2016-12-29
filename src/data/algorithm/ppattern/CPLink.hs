{-|
Module      : Data.Algorithm.PPattern.CPLink
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.CPLink
(
  -- * The @CPLink@ type
  CPLink(..)
, mkCPLink
, mkCPLinkUnsafe

  -- * Querying
, color

  -- * Querying two @CPLink@
, monoChromatic
, biChromatic
, orderConflict
, valueConflict
, monoChromaticOrderConflict
, biChromaticOrderConflict
, monoChromaticValueConflict
, biChromaticValueConflict
)
where

  import qualified Data.Algorithm.PPattern.Point  as Point
  import qualified Data.Algorithm.PPattern.CPoint as CPoint
  import qualified Data.Algorithm.PPattern.Color  as Color

  data CPLink = CPLink { cpP :: {-# UNPACK #-} !CPoint.CPoint
                       , cpQ :: {-# UNPACK #-} !CPoint.CPoint
                       } deriving (Show, Eq)

  {-|
    'mkCPLink' makes a CPLink object from two colored points with the
    the same color.
  -}
  mkCPLink :: CPoint.CPoint -> CPoint.CPoint -> Maybe CPLink
  mkCPLink cpP' cpQ'
    | CPoint.color cpP' /= CPoint.color cpQ' = Nothing
    | otherwise                               = Just (mkCPLinkUnsafe cpP' cpQ')

  {-|
    'mkCPLink' makes a CPLink object from two colored points with the
    the same color.
  -}
  mkCPLinkUnsafe :: CPoint.CPoint -> CPoint.CPoint -> CPLink
  mkCPLinkUnsafe  = CPLink

  {-|
    'color' return the color of a CPLink.
  -}
  color :: CPLink -> Color.Color
  color = CPoint.color . cpP

  {-|
    'sameColor' takes two CPLink objects. It returns True if and only if the
    two links have the same color.
  -}
  monoChromatic :: CPLink -> CPLink -> Bool
  monoChromatic lnk1 lnk2 = color lnk1 == color lnk2

  {-|

  -}
  biChromatic :: CPLink -> CPLink -> Bool
  biChromatic lnk1 lnk2 = not $ monoChromatic lnk1 lnk2

  {-|

  -}
  orderConflict :: CPLink -> CPLink -> Bool
  orderConflict lnk1 lnk2 = x1 < x2 && x1' > x2'
    where
      x1  = Point.xCoord . CPoint.point $ cpP lnk1
      x1' = Point.xCoord . CPoint.point $ cpQ lnk1

      x2  = Point.xCoord . CPoint.point $ cpP lnk2
      x2' = Point.xCoord . CPoint.point $ cpQ lnk2

  {-|
  -}
  monoChromaticOrderConflict :: CPLink -> CPLink -> Bool
  monoChromaticOrderConflict lnk1 lnk2 = monoChromatic lnk1 lnk2 && orderConflict lnk1 lnk2

  {-|
  -}
  biChromaticOrderConflict :: CPLink -> CPLink -> Bool
  biChromaticOrderConflict lnk1 lnk2 = biChromatic lnk1 lnk2 && orderConflict lnk1 lnk2

  {-|

  -}
  valueConflict :: CPLink -> CPLink -> Bool
  valueConflict lnk1 lnk2 = y1 < y2 && y1' > y2'
    where
      y1  = Point.yCoord . CPoint.point $ cpP lnk1
      y1' = Point.yCoord . CPoint.point $ cpQ lnk1

      y2  = Point.yCoord . CPoint.point $ cpP lnk2
      y2' = Point.yCoord . CPoint.point $ cpQ lnk2


  monoChromaticValueConflict :: CPLink -> CPLink -> Bool
  monoChromaticValueConflict l1 l2 = monoChromatic l1 l2 && valueConflict l1 l2

  biChromaticValueConflict :: CPLink -> CPLink -> Bool
  biChromaticValueConflict l1 l2 = monoChromatic l1 l2 && valueConflict l1 l2
