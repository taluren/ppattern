{-|
Module      : Data.Algorithm.PPattern.PMap
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental
ﬁ8
Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.PMap
(
  -- * The @PMap@ type
  PMap(..)
, emptyX
, emptyY

-- * The @PPPMap@ type
, T(..)

  -- * The @PPPMap@ type
, PPPMap(..)

  -- * Querying
, queryNext

  -- * Modifying
, next
, insert
)
where

  import qualified Data.Map.Strict as Map

  import qualified Data.Algorithm.PPattern.Perm  as Perm
  import qualified Data.Algorithm.PPattern.Point as Point

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
  emptyX = PMap { getMap=Map.empty, t=X }

  {-|
  'emptyX' constructs an empty map for x-coordinate comparisons.
  -}
  emptyY :: PMap
  emptyY = PMap { getMap=Map.empty, t=Y }

  {-|
    Insert a key/value in the point map.
  -}
  insert :: Point.Point -> Point.Point -> PMap -> PMap
  insert p p' pm = PMap { getMap=m', t=t pm }
    where
      m  = getMap pm
      m' = Map.insert p p' m

  {-|
    'aboveThreshold pm p val' compare the point 'p' to 'val'.
    For 't=XCoord', the comparison is on the x-coordinate.
    For 't=YCoord', the comparison is on the y-coordinate.
  -}
  aboveThreshold :: T -> Point.Point -> Perm.T -> Bool
  aboveThreshold X p thrshld = Point.xCoord p > thrshld
  aboveThreshold Y p thrshld = Point.yCoord p > thrshld

  {-|
    'updatePMap pm m' "updates" the map of a PMap.
  -}
  updatePMap :: Map.Map Point.Point Point.Point -> PMap -> PMap
  updatePMap m pm = pm { getMap = m }

  {-|
  -}
  afterNext :: Point.Point -> Point.Point -> PMap -> Maybe PPPMap
  afterNext p p' pm
    | p == p'   = Just $ PPPMap (p, p', pm)
    | otherwise = Just $ PPPMap (p, p', pm')
    where
      m   = getMap pm
      m'  = Map.update (\_ -> Just p') p m
      pm' = updatePMap m' pm

  {-|
  -}
  next :: Point.Point -> Perm.T -> PMap -> Maybe PPPMap
  next p thrshld pm = aux p $ getMap pm
    where
      aux :: Point.Point -> Map.Map Point.Point Point.Point -> Maybe PPPMap
      aux p' m = Map.lookup p' m >>= aux'
        where
          aux' :: Point.Point -> Maybe PPPMap
          aux' p''
            | aboveThreshold (t pm) p'' thrshld = afterNext p p'' pm
            | otherwise                         = aux p'' m

  {-|
    Promote 'PPMap'
  -}
  afterQueryNext :: Point.Point -> Point.Point -> PMap -> Maybe PPPMap
  afterQueryNext p p' pm = Just $ PPPMap (p, p', pm)

  {-|

  -}
  queryNext :: Point.Point -> Perm.T -> PMap -> Maybe PPPMap
  queryNext p thrshld pm = aux p $ getMap pm
    where
      aux p' m = Map.lookup p' m >>= aux'
        where
          aux' p''
            | aboveThreshold (t pm) p'' thrshld = afterQueryNext p p'' pm
            | otherwise                         = aux p'' m
