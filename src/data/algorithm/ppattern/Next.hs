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
, mk
, mk'

  -- Querying
, next
, nextK
, xJumpThreshold
, yJumpThreshold

  -- Modifying
, update
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
    Return (if it exists) the next colored point of a colored point.
  -}
  next :: CPoint.CPoint -> Next -> Maybe CPoint.CPoint
  next cp Next { getMap = m } = Map.lookup cp m

  {-|
    Iterate the next function.
  -}
  nextK :: Int -> Next -> CPoint.CPoint -> Maybe CPoint.CPoint
  nextK k n cp
    | k > 0     = next cp n >>= nextK (k-1) n
    | otherwise = Just cp

  {-|
    Update the next mapping for a given colored point.
  -}
  update :: CPoint.CPoint -> CPoint.CPoint -> Next -> Next
  update cp cp' n = Next { getMap = m' }
    where
      m  = getMap n
      m' = case Map.lookup cp m of
             Nothing -> Map.insert cp cp' m
             Just _  -> Map.update (\_ -> Just cp') cp m

  {-|
    Find the minimum number of call to the next function for obtaining a colored
    point with a coordinate above some given threashold.
  -}
  jumpThreshold :: (CPoint.CPoint -> Int) -> Int -> Next -> CPoint.CPoint -> Maybe Int
  jumpThreshold = jumpThresholdAux 1

  -- jumpThreshold auxiliary function
  jumpThresholdAux :: Int -> (CPoint.CPoint -> Int) -> Int -> Next  -> CPoint.CPoint -> Maybe Int
  jumpThresholdAux k f thrshld n cp = Map.lookup cp (getMap n) >>= aux
    where
      aux cp'
        | f cp' > thrshld = Just k
        | otherwise       = jumpThresholdAux (k+1) f thrshld cp' n

  {-|
    jumpThreshold function for x-coordinate.
  -}
  xJumpThreshold :: Int -> Next -> CPoint.CPoint -> Maybe Int
  xJumpThreshold = jumpThreshold CPoint.xCoord

  {-|
    jumpThreshold function for y-coordinate.
  -}
  yJumpThreshold :: Int -> Next -> CPoint.CPoint -> Maybe Int
  yJumpThreshold = jumpThreshold CPoint.y0Coord
