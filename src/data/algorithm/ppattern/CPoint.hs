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

  -- * Fields
, xCoord
, yCoord
, color

  -- * Querying list of colored CPoints
, colors
, nbColors
)
where

  import qualified Data.List   as L
  import qualified Data.Set    as Set
  import qualified Data.Monoid as Monoid

  import qualified Data.Algorithm.PPattern.Color as Color

  newtype CPoint = CPoint (Int, Int, Color.Color) deriving (Eq, Ord)

  instance Show CPoint where
    show (CPoint (x, y, c)) = "("    `Monoid.mappend`
                              "x="   `Monoid.mappend`
                              show x `Monoid.mappend`
                              ","    `Monoid.mappend`
                              "y="   `Monoid.mappend`
                              show y `Monoid.mappend`
                              ","    `Monoid.mappend`
                              "c="   `Monoid.mappend`
                              show c `Monoid.mappend`
                              ")"
  {-|
    'mkPoint'' makes a colored CPoint from two integer coordinates and a color.
  -}
  mkCPoint :: Int -> Int -> Color.Color -> CPoint
  mkCPoint x y c = CPoint (x, y, c)

  {-|
    'xCoord'' returns the x-coordinate of a coloured point.
  -}
  xCoord :: CPoint -> Int
  xCoord (CPoint (x, _, _)) = x

  {-|
    'yCoord'' returns the y-coordinate of a coloured point.
  -}
  yCoord :: CPoint -> Int
  yCoord (CPoint (_, y, _)) = y

  {-|
    'color'' returns the colour of a coloured point.
  -}
  color :: CPoint -> Color.Color
  color (CPoint (_, _, c)) = c

  -- data CPoint = CPoint { xCoord :: {-# UNPACK #-} !T.T
  --                      , yCoord :: {-# UNPACK #-} !T.T
  --                      , color  :: {-# UNPACK #-} !Color.Color
  --                      } deriving (Eq, Ord)
  --
  -- instance Show CPoint where
  --   show CPoint {xCoord=x, yCoord=y, color=c } = "("    `Monoid.mappend`
  --                                                "x="   `Monoid.mappend`
  --                                                show x `Monoid.mappend`
  --                                                ","    `Monoid.mappend`
  --                                                "y="   `Monoid.mappend`
  --                                                show y `Monoid.mappend`
  --                                                ","    `Monoid.mappend`
  --                                                "c="   `Monoid.mappend`
  --                                                show c `Monoid.mappend`
  --                                                ")"
  -- {-|
  --   'mkPoint'' makes a colored CPoint from two integer coordinates and a color.
  -- -}
  -- mkCPoint :: T.T -> T.T -> Color.Color -> CPoint
  -- mkCPoint x y c = CPoint { xCoord=x, yCoord=y, color=c }

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
