{-|
Module      : Data.Algorithm.Next
Structription : Short Structription
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer Structription of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.Next
(
  mkNextIncreasings
)
where

  import qualified Data.List          as L
  import qualified Data.Map.Strict    as Map
  import qualified Data.IntMap.Strict as IntMap
  import qualified Data.Tuple.HT      as THT
  import qualified Data.Foldable      as Fold
  import Data.Maybe

  import qualified Data.Algorithm.NextIncreasings.Permutation  as Permutation
  import qualified Data.Algorithm.NextIncreasings.IntPartition as IntPartition
  import qualified Data.Algorithm.NextIncreasings.Point        as Point
  import qualified Data.Algorithm.NextIncreasings.CPoint       as CPoint
  import qualified Data.Algorithm.NextIncreasings.Color        as Color
  import qualified Data.Algorithm.NextIncreasings.Trgt         as Trgt
  import qualified Data.Algorithm.NextIncreasings.PMap         as PMap
  import qualified Data.Algorithm.NextIncreasings.CMap         as CMap
  import qualified Data.Algorithm.NextIncreasings.CMaps        as CMaps
  import qualified Data.Algorithm.NextIncreasings.CPLink       as CPLink
  import qualified Data.Algorithm.NextIncreasings.State        as State
  import qualified Data.Algorithm.NextIncreasings.Stack        as Stack

  {-|
    'mkNextIncreasing ps' takes a list of points and return a map that associates
    to each point 'p' the smallest point on its right that is greater than 'p'
    (if any).
  -}
  mkNextIncreasing :: [Point.Point] -> PMap.T -> PMap.PMap
  mkNextIncreasing []     PMap.XCoord = PMap.emptyXPMap
  mkNextIncreasing []     PMap.YCoord = PMap.emptyYPMap
  mkNextIncreasing (p:ps) t           = mkNextIncreasingAux ps t s pm
    where
      -- Start with the first point in the stack
      s = Stack.push Stack.empty p

      -- Empty PMap
      pm = case t of
            PMap.XCoord -> PMap.emptyXPMap
            PMap.YCoord -> PMap.emptyYPMap

  mkNextIncreasingAux :: [Point.Point] -> PMap.T -> Stack.Stack Point.Point -> PMap.PMap -> PMap.PMap
  mkNextIncreasingAux []     _ _                  m = m
  mkNextIncreasingAux (p:ps) t s@(Stack.Stack []) m = mkNextIncreasingAux ps t (Stack.push s p) m
  mkNextIncreasingAux ps     t s                  m = mkNextIncreasingAux' ps t s m

  mkNextIncreasingAux' :: [Point.Point] -> PMap.T -> Stack.Stack Point.Point -> PMap.PMap -> PMap.PMap
  mkNextIncreasingAux' ps     t s@(Stack.Stack [])      m = mkNextIncreasingAux ps t s m
  mkNextIncreasingAux' []     _ (Stack.Stack (_ : _)) _ = error "We shouldn't be there"
  mkNextIncreasingAux' (p:ps) t s@(Stack.Stack (p':_))  m
    | v' < v    = mkNextIncreasingAux' (p:ps) t (Stack.popUnsafe s) (PMap.insert p' p m)
    | otherwise = mkNextIncreasingAux ps t (Stack.push s p) m
    where
      f = case t of
            PMap.XCoord -> Point.xCoord
            PMap.YCoord -> Point.yCoord
      v  = f p
      v' = f p'

  {-|
    'mkNextIncreasings' takes a list of colored points. It returns a map that
    associates to each distinct color the next increassing map.
  -}
  mkNextIncreasings :: [CPoint.CPoint] -> PMap.T -> CMap.CMap
  mkNextIncreasings cps t = mkNextIncreasingsAux cps t cs cm
    where
      cs = L.nub $ L.map CPoint.color cps
      cm = CMap.empty

  mkNextIncreasingsAux :: [CPoint.CPoint] -> PMap.T -> [Color.Color] -> CMap.CMap -> CMap.CMap
  mkNextIncreasingsAux _   _   []     cm = cm
  mkNextIncreasingsAux cps t   (c:cs) cm = mkNextIncreasingsAux cps t cs cm'
    where
      cps' = L.filter (\cp -> CPoint.color cp == c) cps
      ps'  = fmap CPoint.point cps'
      pm   = mkNextIncreasing ps' t
      cm'  = CMap.insert c pm cm
