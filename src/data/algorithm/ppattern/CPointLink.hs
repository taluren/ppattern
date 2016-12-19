{-|
Module      : Data.Algorithm.PPattern.CPointCPointLink
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.CPointCPointLink
(
  CPointCPointLink(..)
  --
, mkCPointCPointLink
  --
, color
, sameColor
)
where

  import qualified Data.Algorithm.PPattern.Cpoint as CPoint

  data CPointCPointLink = CPointCPointLink { src  :: CPoint.CPoint
                                           , trgt :: CPoint.CPoint
                                           } deriving (Show, Eq)
  {-|
    'mkCPointCPointLink' makes a CPointCPointLink object from two colored points with the
    the same color.
  -}
  mkCPointCPointLink :: CPoint.CPoint -> CPoint.CPoint -> Maybe CPointCPointLink c
  mkCPointCPointLink src trgt
    | CPointCPointLink.diffC src trgt = Nothing
    | otherwise                 = Just (mkCPointCPointLink src trgt)

  {-|
    'color' return the color of a CPointCPointLink.
  -}
  color :: CPointCPointLink c -> c
  color = CPoint.color . src

  {-|
    'sameColor' takes two CPointCPointLink objects. It returns True if and only if the
    two links have the same color.
  -}
  sameColor :: CPointCPointLink c -> CPointCPointLink c -> Bool
  sameColor cpl1 cpl2 = color cpl1 == color cpl2
