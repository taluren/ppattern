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

  -- _ constructing
, mkCPoint
)
where

  import qualified Data.Monoid as Monoid

  import qualified Data.Algorithm.PPattern.Color as Color

  data CPoint = CPoint { xCoord :: {-# UNPACK #-} !Int
                       , yCoord :: {-# UNPACK #-} !Int
                       , color  :: {-# UNPACK #-} !Color.Color
                       } deriving (Eq)

  -- Show class
  instance Show CPoint where
    show CPoint {xCoord=x, yCoord=y, color=c } =
      "("    `Monoid.mappend`
      "x="   `Monoid.mappend`
      show x `Monoid.mappend`
      ","    `Monoid.mappend`
      "y="   `Monoid.mappend`
      show y `Monoid.mappend`
      ","    `Monoid.mappend`
      "c="   `Monoid.mappend`
      show c `Monoid.mappend`
      ")"
      
  -- Ord clas
  instance Ord CPoint where
    compare cp1 cp2 = compare (xCoord cp1) (xCoord cp2)

  {-|
    'mkPoint'' makes a colored CPoint from two integer coordinates and a color.
  -}
  mkCPoint :: Int -> Int -> Color.Color -> CPoint
  mkCPoint x y c = CPoint { xCoord = x, yCoord = y, color = c }
