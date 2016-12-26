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
  PMap(..)
, emptyXPMap
, emptyYPMap

-- * The @PPPMap@ type
, T(..)

  -- * The @PPPMap@ type
, PPPMap(..)

  -- * Querying
, queryNext

  -- * Modifying
, next
, inser
)
where

  import qualified Data.Map.Strict as Map

  import qualified Data.Algorithm.PPattern.Permutation as Permutation
  import qualified Data.Algorithm.PPattern.Point       as Point

  data T = X | Y
           deriving (Show, Eq, Ord)

  data PMap = PMap { getMap :: Map.Map Point.Point Point.Point
                      , t :: T
                      } deriving (Show)

  -- Propagating update data
  newtype PPPMap = PPPMap (Point.Point, Point.Point, PMap)
                   deriving (Show)

  {-|
  'emptyX' constructs an empty map for x-coordinate comparisons.
  -}
  emptyX :: PMap
  emptyX = PMap { getMap=Map.empty, t=X)

  {-|
  'emptyX' constructs an empty map for x-coordinate comparisons.
  -}
  emptyY :: PMap
  emptyY = PMap { getMap=Map.empty, t=Y)

  {-|
    Insert a key/value in the point map.
  -}
  insert :: Point.Point -> Point.Point -> PMap -> PMap
  insert p p' pm = updatePMap pm . Map.insert p p' $ getMap pm

  {-|
    'compareToVal pm p val' compare the point 'p' to 'val'.
    For 't=XCoord', the comparison is on the x-coordinate.
    For 't=YCoord', the comparison is on the y-coordinate.
  -}
  aboveThreshold :: PMap -> Point.Point -> Permutation.T -> Bool
  aboveThreshold (PMap {t=XCoord}) p val = Point.xCoord p > val
  aboveThreshold (PMap {t=YCoord}) p val = Point.yCoord p > val

  {-|
    'updatePMap pm m' "updates" the map of a PMap.
  -}
  updatePMap :: PMap -> Map.Map Point.Point Point.Point -> PMap
  updatePMap pm m = pm { getMap = m }

  {-|
  -}
  afterNext :: PMap -> Point.Point -> Point.Point -> Maybe PPPMap
  afterNext pm p p'
    | p == p'   = Just $ PPPMap (p, p', pm)
    | otherwise = Just $ PPCMap (p, p', pm')
    where
      m   = getMap pm
      m'  = Map.update (\_ -> Just p') p m
      pm' = updateGetMap m' pm

  {-|
  -}
  next :: Point.Point -> Perm.T -> PMap -> Maybe PPPMap
  next p thrshld pm = aux p (getMap pm)
    where
      aux p' m = Map.lookup p' m >>= aux'
        where
          aux' p''
            | aboveThreshold p'' thrshld (t m) = afterNext pm p p''
            | otherwise                        = aux p'' m

  {-|
    Promote 'PPMap'
  -}
  afterQueryNext :: CMap -> Point.Point -> Point.Point -> Maybe PPCMap
  afterQueryNext cm p p' = Just $ PPPMap (p, p', cm)

  {-|

  -}
  queryNext :: Point.Point -> Perm.T -> CMap -> Maybe PPCMap
  queryNext p thrshld pm = aux p (getMap pm)
    where
      aux p' m = Map.lookup p' m >>= aux'
        where
          aux' p''
            | aboveThreshold p'' thrshld (t m) = afterQueryNext pm p p''
            | otherwise                        = aux p'' m
