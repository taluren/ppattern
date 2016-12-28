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
, getSrc
, getTrgt

  -- * Querying two @CPLink@
, monoChromatic
, biChromatic
, orderConflict
, valueConflict
, monoChromaticOrderConflict
, biChromaticOrderConflict
, monoChromaticValueConflictLG
, monoChromaticValueConflictGL
, biChromaticValueConflictLG
, biChromaticValueConflictGL
)
where

  import qualified Data.Algorithm.PPattern.Perm   as Perm
  import qualified Data.Algorithm.PPattern.Point  as Point
  import qualified Data.Algorithm.PPattern.CPoint as CPoint
  import qualified Data.Algorithm.PPattern.Color  as Color

  data CPLink = CPLink { src  :: {-# UNPACK #-} !CPoint.CPoint
                       , trgt :: {-# UNPACK #-} !CPoint.CPoint
                       } deriving (Show, Eq)

  instance Ord CPLink where
    lnk1 < lnk2 = x1 < x2 && x1' < x2'
      where
        x1  = Point.xCoord . CPoint.point $ src  lnk1
        x1' = Point.xCoord . CPoint.point $ trgt lnk1

        x2  = Point.xCoord . CPoint.point $ src  lnk2
        x2' = Point.xCoord . CPoint.point $ trgt lnk2

  {-|
    'mkCPLink' makes a CPLink object from two colored points with the
    the same color.
  -}
  mkCPLink :: CPoint.CPoint -> CPoint.CPoint -> Maybe CPLink
  mkCPLink srcPoint trgtPoint
    | CPoint.color srcPoint /= CPoint.color trgtPoint = Nothing
    | otherwise                                       = Just (mkCPLinkUnsafe srcPoint trgtPoint)

  {-|
    'mkCPLink' makes a CPLink object from two colored points with the
    the same color.
  -}
  mkCPLinkUnsafe :: CPoint.CPoint -> CPoint.CPoint -> CPLink
  mkCPLinkUnsafe srcPoint trgtPoint = CPLink {src=srcPoint, trgt=trgtPoint}

  {-|
    'color' return the color of a CPLink.
  -}
  color :: CPLink -> Color.Color
  color = CPoint.color . src

  {-|
    'sameColor' takes two CPLink objects. It returns True if and only if the
    two links have the same color.
  -}
  monoChromatic :: CPLink -> CPLink -> Bool
  monoChromatic cpl1 cpl2 = color cpl1 == color cpl2

  {-|

  -}
  biChromatic :: CPLink -> CPLink -> Bool
  biChromatic cpl1 cpl2 = not $ monoChromatic cpl1 cpl2

  {-|

  -}
  orderConflict :: CPLink -> CPLink -> Bool
  orderConflict lnk1 lnk2 = x1 < x2 && x1' > x2'
    where
      x1  = Point.xCoord . CPoint.point $ src  lnk1
      x1' = Point.xCoord . CPoint.point $ trgt lnk1

      x2  = Point.xCoord . CPoint.point $ src  lnk2
      x2' = Point.xCoord . CPoint.point $ trgt lnk2

  {-|
  -}
  monoChromaticOrderConflict :: CPLink -> CPLink -> Bool
  monoChromaticOrderConflict lkn1 lnk2 = monoChromatic lnk1 lnk2 && orderConflict lnk1 lnk2

  {-|
  -}
  biChromaticOrderConflict :: CPLink -> CPLink -> Bool
  biChromaticOrderConflict lkn1 lnk2 = biChromatic lnk1 lnk2 && orderConflict lnk1 lnk2

  {-|

  -}
  valueConflictLG :: CPLink -> CPLink -> Bool
  valueConflict l1 l2 = x1 < x2 && y1 < y2 && y1' > y2'
    where
      x1  = Point.xCoord . CPoint.point $ src  lnk1
      x2' = Point.xCoord . CPoint.point $ trgt lnk2

      y1  = Point.yCoord . CPoint.point $ src  l1
      y1' = Point.yCoord . CPoint.point $ trgt l1

      y2  = Point.yCoord . CPoint.point $ src  l2
      y2' = Point.yCoord . CPoint.point $ trgt l2

  {-|

  -}
  valueConflictGL :: CPLink -> CPLink -> Bool
  valueConflict l1 l2 = x1 < x2 && y1 > y2 && y1' < y2'
    where
      x1  = Point.xCoord . CPoint.point $ src  lnk1
      x2  = Point.xCoord . CPoint.point $ src  lnk2

      y1  = Point.yCoord . CPoint.point $ src  l1
      y1' = Point.yCoord . CPoint.point $ trgt l1

      y2  = Point.yCoord . CPoint.point $ src  l2
      y2' = Point.yCoord . CPoint.point $ trgt l2

  monoChromaticValueConflictLG :: CPLink -> CPLink -> Bool
  monoChromaticValueConflictLG l1 l2 = monoChromatic l1 l2 && valueConflictLG l1 l2

  biChromaticValueConflictGL :: CPLink -> CPLink -> Bool
  biChromaticValueConflictGL l1 l2 = monoChromatic l1 l2 && valueConflictGL l1 l2

  biChromaticValueConflictLG :: CPLink -> CPLink -> Bool
  biChromaticValueConflictLG l1 l2 = biChromatic l1 l2 && valueConflictLG l1 l2

  biChromaticValueConflictGL :: CPLink -> CPLink -> Bool
  biChromaticValueConflictGL l1 l2 = biChromatic l1 l2 && valueConflictGL l1 l2

  {-|

  -}
  get :: (CPLink -> CPoint.CPoint) -> [CPLink] -> Perm.Perm
  get f = Perm.fromListUnsafe . fmap g
    where
      g = Point.yCoord . CPoint.point . f

  {-|

  -}
  getSrc :: [CPLink] -> Perm.Perm
  getSrc = get src

  {-|

  -}
  getTrgt :: [CPLink] -> Perm.Perm
  getTrgt = get trgt
