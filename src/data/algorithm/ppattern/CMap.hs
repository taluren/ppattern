{-|
Module      : Data.Algorithm.PPattern.CMap
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.CMap
(
  -- * The @CMap@ type
  CMap
, empty

  -- * Querying
, nbColors
, next

  -- * Modifying
, updateForNext
, insert
)
where

  import qualified Data.List          as L
  import qualified Data.IntMap.Strict as IntMap

  import qualified Data.Algorithm.PPattern.Perm    as Perm
  import qualified Data.Algorithm.PPattern.Point          as Point
  import qualified Data.Algorithm.PPattern.PMap       as PMap
  import qualified Data.Algorithm.PPattern.Color          as Color

  type CMap = IntMap.IntMap PMap.PMap

  -- Propagating update data
  newtype PPCMap = PPCMap (Point.Point, Point.Point, CMap)
                   deriving (Show)

  {-|
    The empty color map.
  -}
  empty :: CMap
  empty = IntMap.empty

  {-|
    'nbColors m' return the number of colors (i.e. keys) used by the map.
  -}
  nbColors :: CMap -> Int
  nbColors = L.length . IntMap.keys

  {-|
    Delegate insert to IntMap.
  -}
  insert :: Color.Color -> PMap.PMap -> CMap -> CMap
  insert = IntMap.insert

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
  next :: Color.Color -> Point.Point -> Perm.T -> CMap -> Maybe PPCMap
  next c p thrshld cm = IntMap.lookup c cm >>= PMap.next p thrshld >>= afterNext cm

  {-|
    Promote 'PPMap'
  -}
  afterQueryNext :: CMap -> PMap.PPPMap -> Maybe PPCMap
  afterQueryNext cm (PMap.PPMap (p, p', _)) = Just $ PPCMap (p, p', cm)

  {-|

  -}
  queryNext :: Color.Color -> Point.Point -> Perm.T -> CMap -> Maybe PPCMap
  queryNext c p thrshld cms = IntMap.lookup c cm >>= PMap.queryNext p thrshld >>= afterQueryNext cm
