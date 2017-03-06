{-|
Module      : Data.Algorithm.PPattern
Structription : Short Structription
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer Structription of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Context
(
  -- * The @Resolve@ type
  Context

  -- * Constructing
, mk

  -- * Testing
, agree

  -- * Updating
, update
)
where

  import qualified Data.IntMap.Strict as IntMap
  import qualified Data.Tuple         as T

  import qualified Data.Algorithm.PPattern.Color as Color

  -- Context type for constructing increasing colorings of permutation p.
  type Context = (IntMap.IntMap Int, IntMap.IntMap Int)

  precede :: Context -> IntMap.IntMap Int
  precede = T.fst

  follow :: Context -> IntMap.IntMap Int
  follow = T.snd

  mk :: IntMap.IntMap Int -> IntMap.IntMap Int -> Context
  mk precede' follow' = (precede', follow')

  agree :: Color.Color -> Int -> Context -> Bool
  agree c y context = agreeForPrecede && agreeForFollow
    where
      agreeForPrecede = agreePrecede c y (precede context)
      agreeForFollow  = agreeFollow  c y (follow  context)

  agreePrecede :: Color.Color -> Int -> IntMap.IntMap Int -> Bool
  agreePrecede c y m =
    case IntMap.lookup c m of
      Nothing -> True
      Just y' -> y' < y

  agreeFollow :: Color.Color -> Int -> IntMap.IntMap Int -> Bool
  agreeFollow c y m =
    case IntMap.lookup c m of
      Nothing -> True
      Just y' -> y < y'

  update :: Color.Color -> Int -> Context -> Context
  update c y context = mk precede' follow'
    where
      precede' = updatePrecede c y (precede context)
      follow'  = updateFollow  c y (follow  context)

  updatePrecede :: Color.Color -> Int-> IntMap.IntMap Int -> IntMap.IntMap Int
  updatePrecede = aux (1 :: Color.Color)
    where
      aux c refC y m
        | c > refC  = m
        | otherwise = aux (c+1) refC y m'
          where
            m' = case IntMap.lookup c m of
                   Nothing  -> IntMap.insert c y          m
                   Just y'  -> IntMap.insert c (max y y') m

  updateFollow :: Color.Color -> Int -> IntMap.IntMap Int -> IntMap.IntMap Int
  updateFollow c y m =
    case IntMap.lookup c m of
      Nothing       -> m
      Just y'
        | y < y'    -> m
        | y == y'   -> IntMap.delete c m
        | otherwise -> error "updateFollow. We shouldn't be there" -- make ghc -Werror happy
