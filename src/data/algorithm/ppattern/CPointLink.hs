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
, makeCPointLink
  --
, color
)
where

  import qualified Data.Algorithm.PPattern.Cpoint as CPoint

  data CPointLink c = CPointLink { src  :: CPoint.CPoint c
                                 , trgt :: CPoint.CPoint c
                                 } deriving (Show, Eq)
  {-|
    'makeCPointLink' makes a colored point link from two colored points.
  -}
  makeCPointLink src trgt = CPointLink {src=src, trgt=trgt}

  color = CPoint.color . src
