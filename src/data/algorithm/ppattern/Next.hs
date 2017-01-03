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
  mk :: (CPointCPointMap -> Next -> Next) -> [CPoint.CPoint] -> Next -> Next
  mk updateFun cps = updateFun m
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
  mkP = mk updateP

  {-|
    Each color is required to induce an increasing subsequence.
  -}
  mkP' :: [CPoint.CPoint] -> Next
  mkP' = flip mkP empty

  {-|
    Each color is required to induce an increasing subsequence.
  -}
  mkQ :: [CPoint.CPoint] -> Next -> Next
  mkQ = mk updateQ

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
  updateP :: CPointCPointMap -> Next -> Next
  updateP m n = n { pMap=m }

  {-|

  -}
  updateQ :: CPointCPointMap -> Next -> Next
  updateQ m n = n { qMap=m }

  {-|

  -}
  jmpThreshold :: (CPoint.CPoint -> T.T) -> T.T -> CPointCPointMap -> CPoint.CPoint -> Maybe Int
  jmpThreshold = jmpThresholdAux 1

  {-|

  -}
  jmpThresholdAux :: Int -> (CPoint.CPoint -> T.T) -> T.T -> CPointCPointMap -> CPoint.CPoint -> Maybe Int
  jmpThresholdAux k fun thrshld m cp = Map.lookup cp m >>= aux
    where
      aux cp'
        | fun cp' > thrshld = Just k
        | otherwise         = jmpThresholdAux (k+1) fun thrshld m cp'

  {-|

  -}
  jmpThresholdXP :: T.T -> Next -> CPoint.CPoint -> Maybe Int
  jmpThresholdXP thrshld n = jmpThreshold CPoint.xCoord thrshld (pMap n)

  {-|

  -}
  jmpThresholdYP :: T.T -> Next -> CPoint.CPoint -> Maybe Int
  jmpThresholdYP thrshld n = jmpThreshold CPoint.yCoord thrshld (pMap n)

  {-|

  -}
  jmpThresholdXQ :: T.T -> Next -> CPoint.CPoint -> Maybe Int
  jmpThresholdXQ thrshld n = jmpThreshold CPoint.xCoord thrshld (qMap n)

  {-|

  -}
  jmpThresholdYQ :: T.T -> Next -> CPoint.CPoint -> Maybe Int
  jmpThresholdYQ thrshld n = jmpThreshold CPoint.yCoord thrshld (qMap n)

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
