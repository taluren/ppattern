{-|
Module      : Data.Algorithm.PPattern.Interval
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Interval
(
  Rectangle(..)
  --
, makeRectangle
  --
, width
, height
  --
, enclosingRectangle
)
where

  import Data.Maybe

  data Interval a = Interval a a  deriving (Show, Eq)

  instance (Ord a) => Ord (Interval a) where
    (Interval _ r) < (Interval l' _) = l < r'

  makeInterval :: (Ord a) => a -> a -> Maybe (Interval a)
  makeInterval x x'
    | x <= x'   = Just (Interval x x'}
    | otherwise = Nothing

  left :: Interval a -> a
  left l _ = l

  right :: Interval a -> a
  right _ r = r

  intersect :: Interval a -> Interval a -> Bool
  i `intersect` i' = not (i < i' || i > i')
