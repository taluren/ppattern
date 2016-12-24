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

  -- * The @PPPMap@ type
, PPPMap

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

  newtype PMap = PMap (Map.Map Point.Point Point.Point)
                 deriving (Show)

  -- Propagating update data
  newtype PPPMap = PPPMap (Point.Point, Point.Point, PMap)
                   deriving (Show)

  {-|
  'emptyXPMap' constructs an empty map for x-coordinate comparisons.
  -}
  empty :: PMap
  empty = PMap (Map.empty)


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

  {-|
  -}
  afterNext :: CMap -> PMap.PPPMap -> Maybe PPCMap
  afterNext cm (PPMap (p, p', pm))
    | p == p'   = Just $ PPCMap (p, p', cm)
    | otherwise = Just $ PPCMap (p, p', cm')
    where
      cm' = IntMap.update (\_ -> Just p') p cp

  {-|
  -}
  next :: Color.Color -> Point.Point -> Perm.T -> PMap -> PPPMap
  next c p thrshld cm = IntMap.lookup c cm >>= PMap.next p thrshld >>= afterNext cm
