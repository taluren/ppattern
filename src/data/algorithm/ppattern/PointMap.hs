{-|
Module      : Data.Algorithm.PPattern.PointMap
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.PointMap
(
  -- * The @PointMap@ type
  PointMap

  -- * Querying
, next
)
where

  import qualified Data.Map.Strict as Map

  import qualified Data.Algorithm.PPattern.Point as Point

  type PointMap = Map.Map Point.Point Point.Point


  {-|
  -}
  next :: Point.Point -> PointMap -> Maybe (Point.Point)
  next = Map.lookup

  {-|

  -}
  updateForNext :: Point.Point -> Permutation.T -> ColorMap -> Maybe ColorMap
  updateForNext p x m = aux p
    where
      aux p' = Map.lookup p m >>= aux'

      aux' p'
        | Point.xCoord p' > x = Just (Map.update (\_ -> Just p') p m)
        | otherwise           = aux p'
