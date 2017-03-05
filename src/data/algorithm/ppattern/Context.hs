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
  Context(..)

  -- * Constructing
, mk

  -- * Testing
, agree

  -- * Updating
, update
)
where

  import qualified Data.IntMap.Strict as IntMap

  import qualified Data.Algorithm.PPattern.Color as Color

  -- Context type for constructing increasing colorings of permutation p.
  data Context = Context { precede :: IntMap.IntMap Int
                         , follow  :: IntMap.IntMap Int
                         } deriving (Eq, Show)

  mk :: IntMap.IntMap Int -> IntMap.IntMap Int -> Context
  mk m m' = Context { precede = m, follow = m' }

  agree :: Color.Color -> Int -> Context -> Bool
  agree c y context = agreePrecede' && agreeFollow'
    where
      agreePrecede' = agreePrecede c y (precede context)
      agreeFollow'  = agreeFollow  c y (follow context)

  update :: Color.Color -> Int -> Context -> Context
  update c y context = mk precede' follow'
    where
      precede' = updatePrecede c y (precede context)
      follow'  = updateFollow  c y (follow  context)

  agreePrecede :: Color.Color -> IntMap.Key -> IntMap.IntMap Int -> Bool
  agreePrecede c y m =
    case IntMap.lookup c m of
      Nothing -> True
      Just y' -> y' < y

  agreeFollow :: Color.Color -> IntMap.Key -> IntMap.IntMap Int -> Bool
  agreeFollow c y m =
    case IntMap.lookup c m of
      Nothing -> True
      Just y' -> y < y'

  updatePrecede ::
    Color.Color -> IntMap.Key -> IntMap.IntMap Int -> IntMap.IntMap Int
  updatePrecede c = updatePrecedeAux c 1

  updatePrecedeAux ::
    Color.Color -> Color.Color -> IntMap.Key -> IntMap.IntMap Int -> IntMap.IntMap Int
  updatePrecedeAux c c' y m
    | c > c'    = m
    | otherwise = updatePrecedeAux y (c+1) c' m'
      where
        m' = case IntMap.lookup c m of
               Nothing  -> IntMap.insert c y m
               Just y'  -> IntMap.insert (max y y') c m

  updateFollow ::
    Color.Color -> IntMap.Key -> IntMap.IntMap Int -> IntMap.IntMap Int
  updateFollow c y m =
    case IntMap.lookup c m of
      Nothing       -> m
      Just y'
        | y < y'    -> m
        | y == y'   -> IntMap.delete c m
        | otherwise -> error "updateFollow. We shouldn't be there" -- make ghc -Werror happy
