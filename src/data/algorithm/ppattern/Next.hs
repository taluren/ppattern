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
, jmpThresholdXP
, jmpThresholdYP
, jmpThresholdXQ
, jmpThresholdYQ

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

  import qualified Data.Algorithm.PPattern.CPoint as CPoint

  data Next = Next { getMap :: Map.Map CPoint.CPoint CPoint.CPoint }
              deriving (Show, Eq)

  {-|
    Empty next functions.
  -}
  empty :: Next
  empty = Next { getMap = Map.empty }

  {-|
    Make the next mapping for a list of colored points.
  -}
  mk :: [CPoint.CPoint] -> Next
  mk cps = Next { getMap = T.fst $ Foldable.foldr f (Map.empty, IntMap.empty) cps }
    where
      f cp (m, m') = case IntMap.lookup c m' of
                       Nothing  -> (m, IntMap.insert c cp m')
                       Just cp' -> (Map.insert cp cp' m, IntMap.update (\_ -> Just cp) c m')
        where
          c = CPoint.color cp

  {-|
    Each color is required to induce an increasing subsequence.
  -}
  mk' :: [CPoint.CPoint] -> Next
  mk' = flip mk empty


  {-|

  -}
  next :: CPoint.CPoint -> Next -> Maybe CPoint.CPoint
  next cp Next { getMap = m } = Map.lookup cp m

  {-|

  -}
  nextK :: Int -> Next -> CPoint.CPoint -> Maybe CPoint.CPoint
  nextK k m cp
    | k > 0     = next cp m >>= nextK (k-1) m
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
  updateP :: Map.Map CPoint.CPoint CPoint.CPoint -> Next -> Next
  updateP m n = n { pMap=m }

  {-|

  -}
  updateQ :: Map.Map CPoint.CPoint CPoint.CPoint -> Next -> Next
  updateQ m n = n { qMap=m }

  {-|

  -}
  jmpThreshold :: (CPoint.CPoint -> Int) -> Int -> Map.Map CPoint.CPoint CPoint.CPoint -> CPoint.CPoint -> Maybe Int
  jmpThreshold = jmpThresholdAux 1

  {-|

  -}
  jmpThresholdAux :: Int -> (CPoint.CPoint -> Int) -> Int -> Map.Map CPoint.CPoint CPoint.CPoint -> CPoint.CPoint -> Maybe Int
  jmpThresholdAux k fun thrshld m cp = Map.lookup cp m >>= aux
    where
      aux cp'
        | fun cp' > thrshld = Just k
        | otherwise         = jmpThresholdAux (k+1) fun thrshld m cp'

  {-|

  -}
  jmpThresholdXP :: Int -> Next -> CPoint.CPoint -> Maybe Int
  jmpThresholdXP thrshld n = jmpThreshold CPoint.xCoord thrshld (pMap n)

  {-|

  -}
  jmpThresholdYP :: Int -> Next -> CPoint.CPoint -> Maybe Int
  jmpThresholdYP thrshld n = jmpThreshold CPoint.yCoord thrshld (pMap n)

  {-|

  -}
  jmpThresholdXQ :: Int -> Next -> CPoint.CPoint -> Maybe Int
  jmpThresholdXQ thrshld n = jmpThreshold CPoint.xCoord thrshld (qMap n)

  {-|

  -}
  jmpThresholdYQ :: Int -> Next -> CPoint.CPoint -> Maybe Int
  jmpThresholdYQ thrshld n = jmpThreshold CPoint.yCoord thrshld (qMap n)

  {-|

  -}
  insert :: CPoint.CPoint -> CPoint.CPoint -> Map.Map CPoint.CPoint CPoint.CPoint -> Map.Map CPoint.CPoint CPoint.CPoint
  insert = Map.insert

  {-|

  -}
  insertP :: CPoint.CPoint -> CPoint.CPoint -> Next -> Next
  insertP cp1 cp1' n = n { pMap=insert cp1 cp1' (pMap n) }

  {-|

  -}
  insertQ :: CPoint.CPoint -> CPoint.CPoint -> Next -> Next
  insertQ cp1 cp1' n = n { pMap=insert cp1 cp1' (qMap n) }
