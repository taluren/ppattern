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
, orderConflict
, valueConflict
)
where

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

  -}
  orderConflict :: CPLink -> CPLink -> Bool
  orderConflict lnk1 lnk2 = x1 < x2 && x1' > x2'
    where
      x1  = CPoint.xCoord $ cpP lnk1
      x1' = CPoint.xCoord $ cpQ lnk1

      x2  = CPoint.xCoord $ cpP lnk2
      x2' = CPoint.xCoord $ cpQ lnk2

  {-|

  -}
  valueConflict :: CPLink -> CPLink -> Bool
  valueConflict lnk1 lnk2 = y1 < y2 && y1' > y2'
    where
      y1  = CPoint.yCoord $ cpP lnk1
      y1' = CPoint.yCoord $ cpQ lnk1

      y2  = CPoint.yCoord $ cpP lnk2
      y2' = CPoint.yCoord $ cpQ lnk2
