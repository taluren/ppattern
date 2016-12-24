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
    'next c p' calls 'next p' on the 'c'-colored map.
  -}
  next :: Color.Color -> Point.Point -> CMap -> Maybe Point.Point
  next c p m = IntMap.lookup c m >>= PMap.next p

  {-|

  -}
  updateForNext :: Color.Color -> Point.Point -> Perm.T -> CMap -> Maybe CMap
  updateForNext c p val m =  IntMap.lookup c m >>= PMap.updateForNext p val >>= aux
    where
      aux pm = Just (IntMap.update (\_ -> Just pm) c m)

  {-|
    Delegate insert to IntMap.
  -}
  insert :: Color.Color -> PMap.PMap -> CMap -> CMap
  insert = IntMap.insert
