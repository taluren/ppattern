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
, monoChromaticValueConflict
, biChromaticOrderConflict
)
where

  import qualified Data.Algorithm.PPattern.Permutation as Permutation
  import qualified Data.Algorithm.PPattern.Point       as Point
  import qualified Data.Algorithm.PPattern.CPoint      as CPoint
  import qualified Data.Algorithm.PPattern.Color       as Color

  data CPLink = CPLink { src  :: {-# UNPACK #-} !CPoint.CPoint
                               , trgt :: {-# UNPACK #-} !CPoint.CPoint
                               } deriving (Show, Eq)
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
  orderConflict l1 l2 = (x1 < x2 && x1' > x2') || (x1 > x2 && x1' < x2')
    where
      x1  = Point.xCoord . CPoint.point $ src  l1
      x1' = Point.xCoord . CPoint.point $ trgt l1

      x2  = Point.xCoord . CPoint.point $ src  l2
      x2' = Point.xCoord . CPoint.point $ trgt l2

  {-|
  -}
  monoChromaticOrderConflict :: CPLink -> CPLink -> Bool
  monoChromaticOrderConflict l1 l2 = monoChromatic l1 l2 && orderConflict l1 l2

  {-|
  -}
  biChromaticOrderConflict :: CPLink -> CPLink -> Bool
  biChromaticOrderConflict l1 l2 = biChromatic l1 l2 && orderConflict l1 l2

  {-|

  -}
  valueConflict :: CPLink -> CPLink -> Bool
  valueConflict l1 l2 = (y1 < y2 && y1' > y2') || (y1 > y2 && y1' < y2')
    where
      y1  = Point.yCoord . CPoint.point $ src  l1
      y1' = Point.yCoord . CPoint.point $ trgt l1

      y2  = Point.yCoord . CPoint.point $ src  l2
      y2' = Point.yCoord . CPoint.point $ trgt l2

  monoChromaticValueConflict :: CPLink -> CPLink -> Bool
  monoChromaticValueConflict l1 l2 = monoChromatic l1 l2 && valueConflict l1 l2

  biChromaticValueConflict :: CPLink -> CPLink -> Bool
  biChromaticValueConflict l1 l2 = biChromatic l1 l2 && valueConflict l1 l2

  {-|

  -}
  get :: (CPLink -> CPoint.CPoint) -> [CPLink] -> Permutation.Permutation
  get f = Permutation.fromListUnsafe . fmap g
    where
      g = Point.yCoord . CPoint.point . f

  {-|

  -}
  getSrc :: [CPLink] -> Permutation.Permutation
  getSrc = get src

  {-|

  -}
  getTrgt :: [CPLink] -> Permutation.Permutation
  getTrgt = get trgt
