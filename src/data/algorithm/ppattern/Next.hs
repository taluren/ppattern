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
, mkP
, mkP'
, mkQ
, mkQ'

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

  import qualified Data.Tuple             as T
  import qualified Data.Map.Strict        as Map
  import qualified Data.IntMap.Strict     as IntMap
  import qualified Data.Foldable          as Foldable

  import qualified Data.Algorithm.PPattern.Types  as T
  import qualified Data.Algorithm.PPattern.CPoint as CPoint
  import qualified Data.Algorithm.PPattern.Color  as Color

  type CPointCPointMap = Map.Map CPoint.CPoint CPoint.CPoint

  data Next = Next { pMap :: !CPointCPointMap -- ^ next function for permutation p
                   , qMap :: !CPointCPointMap -- ^ next function for permutation q
                   } deriving (Show)

  {-|
    Empty next functions.
  -}
  empty :: Next
  empty = Next { pMap=Map.empty, qMap=Map.empty }

  {-|
  -}
  make :: (CPointCPointMap -> Next -> Next) -> [CPoint.CPoint] -> Next -> Next
  make updateFun cps = updateFun m
    where
      m = T.fst $ Foldable.foldr f (Map.empty, IntMap.empty) cps

  f :: CPoint.CPoint -> (CPointCPointMap, IntMap.IntMap CPoint.CPoint) -> (CPointCPointMap, IntMap.IntMap CPoint.CPoint)
  f cp (m, nextCP) = fAux (IntMap.lookup (Color.toInt c) nextCP)
    where
      c = CPoint.color cp

      fAux :: Maybe CPoint.CPoint -> (CPointCPointMap, IntMap.IntMap CPoint.CPoint)
      fAux Nothing = (m, nextCP')
        where
          nextCP' = IntMap.insert (Color.toInt c) cp nextCP
      fAux (Just cp') = (m', nextCP')
        where
          m'      = Map.insert cp cp' m
          nextCP' = IntMap.update (\_ -> Just cp) (Color.toInt c) nextCP

  {-|
    Each color is required to induce an increasing subsequence.
  -}
  mkP :: [CPoint.CPoint] -> Next -> Next
  mkP = make updateP

  {-|
    Each color is required to induce an increasing subsequence.
  -}
  mkP' :: [CPoint.CPoint] -> Next
  mkP' = flip mkP empty

  {-|
    Each color is required to induce an increasing subsequence.
  -}
  mkQ :: [CPoint.CPoint] -> Next -> Next
  mkQ = make updateQ

  {-|
    Each color is required to induce an increasing subsequence.
  -}
  mkQ' :: [CPoint.CPoint] -> Next
  mkQ' = flip mkQ empty

  {-|

  -}
  next :: CPoint.CPoint -> CPointCPointMap-> Maybe CPoint.CPoint
  next = Map.lookup

  {-|

  -}
  nextP :: CPoint.CPoint -> Next-> Maybe CPoint.CPoint
  nextP p n = next p (pMap n)

  {-|

  -}
  nextQ :: CPoint.CPoint -> Next-> Maybe CPoint.CPoint
  nextQ p n = next p (pMap n)

  {-|

  -}
  nextK :: Int -> CPointCPointMap -> CPoint.CPoint -> Maybe CPoint.CPoint
  nextK k m cp
    | k > 1     = next cp m >>= nextK (k-1) m
    | otherwise = Just cp

  {-|

  -}
  nextPK :: Int -> Next -> CPoint.CPoint -> Maybe CPoint.CPoint
  nextPK k n = nextK k (pMap n)

  {-|

  -}
  nextQK :: Int -> Next -> CPoint.CPoint -> Maybe CPoint.CPoint
  nextQK k n = nextK k (qMap n)

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
  jumpThreshold :: (CPoint.CPoint -> T.T) -> T.T -> CPointCPointMap -> CPoint.CPoint -> Maybe Int
  jumpThreshold = jumpThresholdAux 1

  {-|

  -}
  jumpThresholdAux :: Int -> (CPoint.CPoint -> T.T) -> T.T -> CPointCPointMap -> CPoint.CPoint -> Maybe Int
  jumpThresholdAux k fun thrshld m cp = Map.lookup cp m >>= aux
    where
      aux cp'
        | fun cp' > thrshld = Just k
        | otherwise       = jumpThresholdAux (k+1) fun thrshld m cp'

  {-|

  -}
  jumpThresholdXP :: T.T -> Next -> CPoint.CPoint -> Maybe Int
  jumpThresholdXP thrshld n = jumpThreshold CPoint.xCoord thrshld (pMap n)

  {-|

  -}
  jumpThresholdYP :: T.T -> Next -> CPoint.CPoint -> Maybe Int
  jumpThresholdYP thrshld n = jumpThreshold CPoint.yCoord thrshld (pMap n)

  {-|

  -}
  jumpThresholdXQ :: T.T -> Next -> CPoint.CPoint -> Maybe Int
  jumpThresholdXQ thrshld n = jumpThreshold CPoint.xCoord thrshld (qMap n)

  {-|

  -}
  jumpThresholdYQ :: T.T -> Next -> CPoint.CPoint -> Maybe Int
  jumpThresholdYQ thrshld n = jumpThreshold CPoint.yCoord thrshld (qMap n)

  {-|

  -}
  insert :: CPoint.CPoint -> CPoint.CPoint -> CPointCPointMap -> CPointCPointMap
  insert = Map.insert

  {-|

  -}
  insertP :: CPoint.CPoint -> CPoint.CPoint -> Next -> Next
  insertP cp1 cp1' n = n { pMap=insert cp1 cp1' (pMap n) }

  {-|

  -}
  insertQ :: CPoint.CPoint -> CPoint.CPoint -> Next -> Next
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
