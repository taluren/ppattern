{-|
Module      : Data.Algorithm.PPattern.PMap
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.PMap
(
  -- * The @PMap@ type
  PMap
, emptyXPMap
, emptyYPMap

  -- * The @PMapType@ type
, T(..)

  -- * Querying
, next

  -- * Modifying
, updateForNext
, insert
)
where

  import qualified Data.Map.Strict as Map

  import qualified Data.Algorithm.PPattern.Permutation as Permutation
  import qualified Data.Algorithm.PPattern.Point       as Point

  data T = XCoord | YCoord
           deriving (Show, Eq, Ord)

  data PMap = PMap { getMap :: Map.Map Point.Point Point.Point
                   , t      :: T
                   } deriving (Show)

  {-|
  'emptyXPMap' constructs an empty map for x-coordinate comparisons.
  -}
  emptyXPMap :: PMap
  emptyXPMap = PMap { getMap=Map.empty, t=XCoord }

  {-|
  'emptyYPMap' constructs an empty map for y-coordinate comparisons.
  -}
  emptyYPMap :: PMap
  emptyYPMap = PMap { getMap=Map.empty, t=YCoord }

  {-|
   'next p m' returns the next point in the map (i.e., the smallest point for is
   greater than 'p')
  -}
  next :: Point.Point -> PMap -> Maybe Point.Point
  next p = Map.lookup p . getMap

  {-|
    'compareToVal pm p val' compare the point 'p' to 'val'.
    For 't=XCoord', the comparison is on the x-coordinate.
    For 't=YCoord', the comparison is on the y-coordinate.
  -}
  compareToVal :: PMap -> Point.Point -> Permutation.T -> Bool
  compareToVal (PMap {t=XCoord}) p val = Point.xCoord p > val
  compareToVal (PMap {t=YCoord}) p val = Point.yCoord p > val

  {-|
    'updatePMap pm m' "updates" the map of a PMap.
  -}
  updatePMap :: PMap -> Map.Map Point.Point Point.Point -> PMap
  updatePMap pm m = pm { getMap = m }

  {-|
    'updateForNext p val pm'
  -}
  updateForNext :: Point.Point -> Permutation.T -> PMap -> Maybe PMap
  updateForNext p val pm = aux (getMap pm) p
    where
      -- Find next point
      aux m p'= Map.lookup p' m >>= aux' m

      -- Return the next point if it is greater than the requested val.
      aux' m p'
        | compareToVal pm p val = Just $ updatePMap pm (Map.update (\_ -> Just p') p m)
        | otherwise             = aux m p'

  {-|
    Insert a key/value in the point map.
  -}
  insert :: Point.Point -> Point.Point -> PMap -> PMap
  insert p p' pm = updatePMap pm . Map.insert p p' $ getMap pm
