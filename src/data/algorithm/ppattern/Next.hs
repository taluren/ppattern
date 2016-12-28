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
  -- * The @Next@ type
  Next
, empty
, makeP
, makeQ

  -- Querying
, nextP
, nextQ
, nextPK
, nextQK
, jumpThresholdXP
, jumpThresholdYP
, jumpThresholdXQ
, jumpThresholdYQ

  -- Modifying
, updateP
, updateQ
, insertP
, insertQ
)
where

  import qualified Data.List   as L
  import qualified Data.IntSet as IntSet
  import qualified Data.Function.Flippers as Flippers

  import qualified Data.Algorithm.PPattern.Point  as Point
  import qualified Data.Algorithm.PPattern.CPoint as CPoint
  import qualified Data.Algorithm.PPattern.Color  as Color
  import qualified Data.Algorithm.PPattern.PMap   as PMap
  import qualified Data.Algorithm.PPattern.CMap   as CMap
  import qualified Data.Algorithm.PPattern.Stack  as Stack

  type CPointCPointMap = Map.Map CPoint.CPoint CPoint.CPoint

  data Next = Next { pMap :: CPointCPointMap
                   , qMap :: CPointCPointMap
                   } deriving (Show)

  {-|
    Empty next mapping.
  -}
  empty :: Next
  empty = Next { pMap=Map.empty, qMap=Map.empty }

  {-|
    Empty next mapping.
  -}
  make :: (CPointCPointMap -> Next -> Next) -> [CPoint.CPoint] -> Next -> Next
  make cps n updateFun = updateFun m n
    where
      m = T.fst $ Fold.foldr f (Map.empty, IntMap.empty) cps

      f cp (m, nextCP) = fAux (IntMap.lookup c nextCP)
        where
          c = CPoint.color cp

          fAux Nothing = (m, nextCP')
            where
              nextCP' = IntMap.insert c cp nextCP
          fAux (Just cp') = (m', nextCP')
            where
              m'      = Map.insert cp cp' m
              nextCP' = IntMap.update (\_ -> Just cp) c nextCP

  {-|
    Each color is required to induce an increasing subsequence.
  -}
  makeP :: [CPoint.CPoint] -> Next -> Next
  makeP = make updateP

  {-|
    Each color is required to induce an increasing subsequence.
  -}
  makeP' :: [CPoint.CPoint] -> Next
  makeP' = flip makeP empty

  {-|
    Each color is required to induce an increasing subsequence.
  -}
  makeQ :: [CPoint.CPoint] -> Next -> Next
  makeQ = make updateQ

  {-|
    Each color is required to induce an increasing subsequence.
  -}
  makeQ' :: [CPoint.CPoint] -> Next
  makeQ' = flip makeQ empty


  {-|

  -}
  nextP :: CPoint.CPoint -> Next-> Maybe CPoint.CPoint
  nextP p n = Map.lookup p (pMap n)

  {-|

  -}
  nextQ :: CPoint.CPoint -> Next-> Maybe CPoint.CPoint
  nextQ p n = Map.lookup p (pMap n)

  {-|

  -}
  nextK :: CPoint.CPoint -> Int -> CPointCPointMap -> Maybe CPoint.CPoint
  nextK cp k m
    | k > 1     = next cp m >>= Flippers.rotate3 nextK (k-1) m
    | otherwise = Just cp

  {-|

  -}
  nextPK :: CPoint.CPoint -> Int -> Next -> Maybe CPoint.CPoint
  nextPK cp k n = nextK cp k (pMap n)

  {-|

  -}
  nextQK :: CPoint.CPoint -> Int -> Next -> Maybe CPoint.CPoint
  nextQK cp k n = nextK cp k (qMap n)

  {-|

  -}
  updateP :: CPointCPointMap -> Next -> Next
  updateP m n = n { pMap=m }

  {-|

  -}
  updateQ :: CPointCPointMap -> Next -> Next
  updateQ m n = n { qMap=m }

  {-|

  -}
  jumpThreshold :: (CPoint.CPoint -> Int) -> Int -> CPointCPointMap -> CPoint.CPoint -> Maybe Int
  jumpThreshold = jumpThresholdAux 1

  {-|

  -}
  jumpThresholdAux :: Int -> (CPoint.CPoint -> Int) -> Int -> CPointCPointMap -> CPoint.CPoint -> Maybe Int
  jumpThresholdAux k f thrshld m cp = Map.lookup cp m >>= aux
    where
      aux cp'
        | f cp' > thrshld = Just k
        | otherwise       = jumpThresholdAux (k+1) f thrshld m cp'

  {-|

  -}
  jumpThresholdXP :: Int -> Next -> CPoint.CPoint -> Maybe Int
  jumpThresholdXP thrshld n = jumpThreshold CPoint.xCoord thrshld (pMap n)

  {-|

  -}
  jumpThresholdYP :: Int -> Next -> CPoint.CPoint -> Maybe Int
  jumpThresholdYP thrshld n = jumpThreshold CPoint.yCoord thrshld (pMap n)

  {-|

  -}
  jumpThresholdXQ :: Int -> Next -> CPoint.CPoint -> Maybe Int
  jumpThresholdXQ thrshld n = jumpThreshold CPoint.xCoord thrshld (qMap n)

  {-|

  -}
  jumpThresholdYQ :: Int -> Next -> CPoint.CPoint -> Maybe Int
  jumpThresholdYQ thrshld n = jumpThreshold CPoint.yCoord thrshld (qMap n)

  {-|

  -}
  nextAboveYThreshold :: Int -> Next -> CPoint.CPoint -> Maybe Int
  nextAboveYThreshold = jumpThreshold CPoint.yCoord

  {-|

  -}
  insert :: Cpoint.CPoint -> CPoint->Cpoint -> CPointCPointMap -> CPointCPointMap
  insert = Map.insert

  {-|

  -}
  insertP :: Cpoint.CPoint -> CPoint->Cpoint -> Next -> Next
  insertP cp1 cp1' n = n { pMap=insert cp1 cp1' (pMap n) }

  {-|

  -}
  insertQ :: Cpoint.CPoint -> CPoint->Cpoint -> Next -> Next
  insertQ cp1 cp1' n = n { pMap=insert cp1 cp1' (qMap n) }

  --
  -- {-|
  --   'mkNextIncreasing ps' takes a list of points and return a map that associates
  --   to each point 'p' the smallest point on its right that is greater than 'p'
  --   (if any).
  -- -}
  -- mkNextIncreasing :: [Point.Point] -> PMap.T -> PMap.PMap
  -- mkNextIncreasing []     PMap.X = PMap.emptyX
  -- mkNextIncreasing []     PMap.Y = PMap.emptyY
  -- mkNextIncreasing (p:ps) t      = mkNextIncreasingAux ps t s pm
  --   where
  --     -- empty map for x-ccordinate
  --     pm = case t of
  --       PMap.X -> PMap.emptyX
  --       PMap.Y -> PMap.emptyY
  --
  --     -- Start with the first point on the stack
  --     s = Stack.push Stack.empty p
  --
  -- mkNextIncreasingAux :: [Point.Point] -> PMap.T -> Stack.Stack Point.Point -> PMap.PMap -> PMap.PMap
  -- mkNextIncreasingAux []     _ _                  m = m
  -- mkNextIncreasingAux (p:ps) t s@(Stack.Stack []) m = mkNextIncreasingAux ps t (Stack.push s p) m
  -- mkNextIncreasingAux ps     t s                  m = mkNextIncreasingAux' ps t s m
  --
  -- mkNextIncreasingAux' :: [Point.Point] -> PMap.T -> Stack.Stack Point.Point -> PMap.PMap -> PMap.PMap
  -- mkNextIncreasingAux' ps     t s@(Stack.Stack [])      m = mkNextIncreasingAux ps t s m
  -- mkNextIncreasingAux' []     _ (Stack.Stack (_ : _)) _ = error "We shouldn't be there"
  -- mkNextIncreasingAux' (p:ps) t s@(Stack.Stack (p':_))  m
  --   | v' < v    = mkNextIncreasingAux' (p:ps) t (Stack.popUnsafe s) (PMap.insert p' p m)
  --   | otherwise = mkNextIncreasingAux ps t (Stack.push s p) m
  --   where
  --     f = case t of
  --           PMap.X -> Point.xCoord
  --           PMap.Y -> Point.yCoord
  --     v  = f p
  --     v' = f p'
  --
  -- {-|
  --   'mkNextIncreasings' takes a list of colored points. It returns a map that
  --   associates to each distinct color the next increassing map.
  -- -}
  -- mkNextIncreasings :: [CPoint.CPoint] -> PMap.T -> CMap.CMap
  -- mkNextIncreasings cps t = mkNextIncreasingsAux cps t cs cm
  --   where
  --     -- collect colors
  --     cs = IntSet.toList . IntSet.fromList $ L.map CPoint.color cps
  --
  --     -- initial empty map
  --     cm = CMap.empty
  --
  -- mkNextIncreasingsAux :: [CPoint.CPoint] -> PMap.T -> [Color.Color] -> CMap.CMap -> CMap.CMap
  -- mkNextIncreasingsAux _   _   []     cm = cm
  -- mkNextIncreasingsAux cps t   (c:cs) cm = mkNextIncreasingsAux cps t cs cm'
  --   where
  --     -- filter colored points by color
  --     cps' = L.filter (\cp -> CPoint.color cp == c) cps
  --
  --     -- collect points
  --     ps'  = fmap CPoint.point cps'
  --
  --     -- compute and store next function for color c and points ps'
  --     pm   = mkNextIncreasing ps' t
  --     cm'  = CMap.insert c pm cm
