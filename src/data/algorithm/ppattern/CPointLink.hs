{-|
Module      : Data.Algorithm.PPattern.CPointLink
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.CPointLink
(
  CPointLink(..)
  --
, mkCPointLink
  --
, color
, sameColor
)
where

  import qualified Data.Algorithm.PPattern.Cpoint as CPoint

  data CPointLink = CPointLink { src  :: CPoint.CPoint
                               , trgt :: CPoint.CPoint
                               } deriving (Show, Eq)
  {-|
    'mkCPointLink' makes a CPointLink object from two colored points with the
    the same color.
  -}
  mkCPointLink :: CPoint.CPoint -> CPoint.CPoint -> Maybe CPointLink c
  mkCPointLink src trgt
    | CPoint.color src /= CPoint.color trgt = Nothing
    | otherwise                             = CPointLink {src=src, trgt=trgt}

  {-|
    'color' return the color of a CPointLink.
  -}
  color :: CPointLink c -> c
  color = CPoint.color . src

  {-|
    'sameColor' takes two CPointLink objects. It returns True if and only if the
    two links have the same color.
  -}
  sameColor :: CPointLink c -> CPointLink c -> Bool
  sameColor cpl1 cpl2 = color cpl1 == color cpl2
