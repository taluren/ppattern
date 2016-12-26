{-|
Module      : Data.Algorithm.PPattern.Next
Structription : Short Structription
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer Structription of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Next
(
  mkNextIncreasings
)
where

  import qualified Data.List   as L
  import qualified Data.IntSet as IntSet

  import qualified Data.Algorithm.PPattern.Point  as Point
  import qualified Data.Algorithm.PPattern.CPoint as CPoint
  import qualified Data.Algorithm.PPattern.Color  as Color
  import qualified Data.Algorithm.PPattern.PMap   as PMap
  import qualified Data.Algorithm.PPattern.CMap   as CMap
  import qualified Data.Algorithm.PPattern.Stack  as Stack

  {-|
    'mkNextIncreasing ps' takes a list of points and return a map that associates
    to each point 'p' the smallest point on its right that is greater than 'p'
    (if any).
  -}
  mkNextIncreasing :: [Point.Point] -> PMap.T -> PMap.PMap
  mkNextIncreasing []     PMap.X = PMap.emptyX
  mkNextIncreasing []     PMap.Y = PMap.emptyY
  mkNextIncreasing (p:ps) t      = mkNextIncreasingAux ps t s pm
    where
      -- empty map for x-ccordinate
      pm = case t of
        PMap.X -> PMap.emptyX
        PMap.Y -> PMap.emptyY

      -- Start with the first point on the stack
      s = Stack.push Stack.empty p

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
            PMap.X -> Point.xCoord
            PMap.Y -> Point.yCoord
      v  = f p
      v' = f p'

  {-|
    'mkNextIncreasings' takes a list of colored points. It returns a map that
    associates to each distinct color the next increassing map.
  -}
  mkNextIncreasings :: [CPoint.CPoint] -> PMap.T -> CMap.CMap
  mkNextIncreasings cps t = mkNextIncreasingsAux cps t cs cm
    where
      -- collect colors
      cs = IntSet.toList . IntSet.fromList $ L.map CPoint.color cps

      -- initial empty map
      cm = CMap.empty

  mkNextIncreasingsAux :: [CPoint.CPoint] -> PMap.T -> [Color.Color] -> CMap.CMap -> CMap.CMap
  mkNextIncreasingsAux _   _   []     cm = cm
  mkNextIncreasingsAux cps t   (c:cs) cm = mkNextIncreasingsAux cps t cs cm'
    where
      -- filter colored points by color
      cps' = L.filter (\cp -> CPoint.color cp == c) cps

      -- collect points
      ps'  = fmap CPoint.point cps'

      -- compute and store next function for color c and points ps'
      pm   = mkNextIncreasing ps' t
      cm'  = CMap.insert c pm cm
