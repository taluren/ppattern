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
, emptyXPointMap
, emptyYPointMap

  -- * The @PointMapType@ type
, PointMapType(..)

  -- * Querying
, next

  -- * Updating
, updateForNext
)
where

  import qualified Data.Map.Strict as Map

  import qualified Data.Algorithm.PPattern.Permutation    as Permutation
  import qualified Data.Algorithm.PPattern.Point          as Point
  import qualified Data.Algorithm.PPattern.CoordSelection as CoordSelection

  data PointMapType = XCoord | YCoord

  data PointMap = PointMap { getMap :: Map.Map Point.Point Point.Point
                           , t      :: !PointMapType
                           } deriving (Show)

   {-|
    'emptyXPointMap' constructs an empty map for x-coordinate comparisons.
   -}
   emptyXPointMap :: PointMap
   emptyXPointMap = PointMap {getMap=Map.empty, t=XCoord}

   {-|
   'emptyYPointMap' constructs an empty map for y-coordinate comparisons.
   -}
   emptyYPointMap :: PointMap
   emptyYPointMap = PointMap {getMap=Map.empty, t=YCoord}

  {-|
   'next p m' returns the next point in the map (i.e., the smallest point for is
   greater than 'p')
  -}
  next :: Point.Point -> PointMap a -> Maybe Point.Point
  next p = Map.lookup p . getMap

  {-|
    'compareToVal pm p val' compare the point 'p' to 'val'.
    For 't=XCoord', the comparison is on the x-coordinate.
    For 't=YCoord', the comparison is on the y-coordinate.
  -}
  compareToVal :: PointMap -> Point.Point -> Permutation.T -> Bool
  compareToVal (PointMap {t=XCoord}) p val = Point.xCoord p > val
  compareToVal (PointMap {t=YCoord}) p val = Point.yCoord p > val

  {-|
    'updateForNext p val pm'
  -}
  updateForNext :: Point.Point -> Permutation.T -> PointMap CoordSelection.XCoord-> Maybe PointMap
  updateForNext p val pm = aux p (getMap pm)
    where
      -- Find next point
      aux p' m = Map.lookup p' m >>= aux'

      -- Return the next point if it is greater than the requested val.
      aux' p'
        | compareToVal pm p val = Just (Map.update (\_ -> Just p') p m)
        | otherwise             = aux p'
