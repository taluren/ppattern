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

module Data.Algorithm.PPattern
(
  search
, resolve1
)
where

  import qualified Data.List       as L
  import qualified Data.Map.Strict as Map
  import qualified Data.Tuple.HT   as THT
  import qualified Data.Foldable   as Fold
  import Data.Maybe

  import qualified Data.Algorithm.PPattern.Permutation  as Permutation
  import qualified Data.Algorithm.PPattern.IntPartition as IntPartition
  import qualified Data.Algorithm.PPattern.Point        as Point
  import qualified Data.Algorithm.PPattern.CPoint       as CPoint
  import qualified Data.Algorithm.PPattern.Color        as Color
  import qualified Data.Algorithm.PPattern.Trgt         as Trgt
  import qualified Data.Algorithm.PPattern.PointMap     as PointMap
  import qualified Data.Algorithm.PPattern.ColorMap     as ColorMap
  import qualified Data.Algorithm.PPattern.CPointLink   as CPointLink
  import qualified Data.Algorithm.PPattern.State        as State
  import qualified Data.Algorithm.PPattern.Stack        as Stack

  {-|
    'mkNextIncreasing ps' takes a list of points and return a map that associates
    to each point 'p' the smallest point on its right that is greater than 'p'
    (if any).
  -}
  mkNextIncreasing :: [Point.Point] -> PointMap.PointMap
  mkNextIncreasing []     = Map.empty
  mkNextIncreasing (p:ps) = mkNextIncreasingAux ps s m
    where
      s = Stack.push Stack.empty p
      m = Map.empty

  {-|
    Helper functions for 'mkNextIncreasing'.
  -}
  mkNextIncreasingAux :: [Point.Point] -> Stack.Stack Point.Point -> PointMap.PointMap -> PointMap.PointMap
  mkNextIncreasingAux []     _                  m = m
  mkNextIncreasingAux (p:ps) s@(Stack.Stack []) m = mkNextIncreasingAux ps (Stack.push s p) m
  mkNextIncreasingAux ps     s                  m = mkNextIncreasingAux' ps s m

  mkNextIncreasingAux' :: [Point.Point] -> Stack.Stack Point.Point -> PointMap.PointMap -> PointMap.PointMap
  mkNextIncreasingAux' ps     s@(Stack.Stack [])     m = mkNextIncreasingAux ps s m
  mkNextIncreasingAux' []     (Stack.Stack (_ : _))  _ = error "We shouldn't be there"
  mkNextIncreasingAux' (p:ps) s@(Stack.Stack (p':_)) m
    | x' < x    = mkNextIncreasingAux' (p:ps) (Stack.popUnsafe s) (Map.insert p' p m)
    | otherwise = mkNextIncreasingAux ps (Stack.push s p) m
    where
      x  = Point.xCoord p
      x' = Point.xCoord p'

  {-|
    'mkNextIncreasings' takes a list of colored points. It returns a map that
    associates to each distinct color the next increassing map to this color.
  -}
  mkNextIncreasings :: [CPoint.CPoint] -> ColorMap.ColorMap
  mkNextIncreasings cps = mkNextIncreasingsAux cps cs m
    where
      cs = L.nub $ L.map CPoint.color cps
      m  = Map.empty

  {-|

  -}
  mkNextIncreasingsAux :: [CPoint.CPoint] -> [Color.Color] -> ColorMap.ColorMap -> ColorMap.ColorMap
  mkNextIncreasingsAux _ []     m = m
  mkNextIncreasingsAux cps (c:cs) m = mkNextIncreasingsAux cps cs m'
    where
      cps' = L.filter (\cp -> CPoint.color cp == c) cps
      ps'  = fmap CPoint.point cps'
      cm   = mkNextIncreasing ps'
      m'   = Map.insert c cm m

  {-|
    The 'mkCPoints' function takes a permutation and a color mapping given
    in the form of a map, and it return a list of CPoint.
  -}
  mkCPoints :: Permutation.Permutation -> Map.Map Permutation.T Color.Color -> [CPoint.CPoint]
  mkCPoints (Permutation.Permutation xs) m = L.map (THT.uncurry3 CPoint.mkCPoint') t3s
    where
      cs  = Fold.foldr (\x acc -> (fromJust (Map.lookup x m)):acc) [] xs
      t3s = L.zip3 [1..] xs cs

  {-|
    'leftmostCPointMapping cp1s cp2s' return the leftmost color friendly mapping
    from 'cp1s' to 'cp2s'.
  -}
  leftmostCPointMapping :: [CPoint.CPoint] -> [CPoint.CPoint] -> Maybe [CPointLink.CPointLink]
  leftmostCPointMapping [] []  = Just []
  leftmostCPointMapping [] _   = Just []
  leftmostCPointMapping _  []  = Nothing
  leftmostCPointMapping (cp1:cp1s) (cp2:cp2s)
    | c1 == c2   = CPointLink.mkCPointLink cp1 cp2 >>= \cpl -> fmap (cpl:) (leftmostCPointMapping cp1s cp2s)
    | otherwise = leftmostCPointMapping (cp1:cp1s) cp2s
    where
      c1 = CPoint.color cp1
      c2 = CPoint.color cp2

  {-|
    'mapFromPartition' transform an integer partition into a map object that
    associates to each each element a color.
  -}
  mapFromPartition :: [Permutation.Permutation] -> Map.Map Permutation.T Color.Color
  mapFromPartition = Map.fromList . Fold.concatMap f . flip zip [1..] . fmap Permutation.toList
    where
      colors i  = L.repeat $ Color.mkColor i

      f (xs, i) = L.zip xs $ colors i

  {-|
    'toIntPartition ps' returns the lengths of all partition.
  -}
  toIntPartition :: [Permutation.Permutation] -> IntPartition.IntPartition
  toIntPartition ps = IntPartition.mkIntPartition ls'
    where
      ls  = fmap Permutation.size ps
      ls' = L.sortBy (flip compare) ls

  {-|
    'mkSrc p k' returns all 'k'-coloring of permutation 'p', where each coloring
    is an increasing subsequence.
  -}
  mkSrc :: Permutation.Permutation -> Int -> IntPartition.IntPartition -> [[CPoint.CPoint]]
  mkSrc p k ip = aux pis
    where
      pis = Permutation.partitionsIncreasings p k

      aux []= []
      aux (partition:partitions)
        | IntPartition.compatible ip2 ip = cps:aux partitions
        | otherwise                      = aux partitions
        where
          ip2 = toIntPartition partition
          cps  = mkCPoints p (mapFromPartition partition)

  {-|
    the 'mkTrgt' function is in charge of finding a k-increasing-coloring
    of a given permutation. It returns the next function for each color in the
    form of a map.
  -}
  mkTrgt :: Permutation.Permutation -> (Trgt.Trgt, IntPartition.IntPartition)
  mkTrgt p = (Trgt.mkTrgt cps next, toIntPartition partition)
    where
      partition = Permutation.greedyPartitionIncreasings1 p
      cps       = mkCPoints p (mapFromPartition partition)
      next      = mkNextIncreasings cps

  {-|
    The 'search' function takes two permutations 'p' and 'q', and it returns
    (if possible) an order-isomorphic occurrence of 'p' in 'q'.
  -}
  search :: Permutation.Permutation -> Permutation.Permutation -> Maybe [CPointLink.CPointLink]
  search p q = searchAux srcs trgt
    where
      -- make target structure
      (trgt, ip) = mkTrgt q

      -- make all source structures
      srcs = mkSrc p (Trgt.nbColors trgt) ip

  searchAux :: [[CPoint.CPoint]] -> Trgt.Trgt -> Maybe [CPointLink.CPointLink]
  searchAux []               _    = Nothing
  searchAux (srcCps:srcCpss) trgt = case doSearch srcCps trgt of
                                      Nothing -> searchAux srcCpss trgt
                                      Just q  -> Just q

  {-|

  -}
  doSearch :: [CPoint.CPoint] -> Trgt.Trgt -> Maybe [CPointLink.CPointLink]
  doSearch src trgt = ls >>= mkState >>= doSearchAux
    where
      ls          = leftmostCPointMapping src (Trgt.cPoints trgt)
      mkState ls' = Just (State.mkState (Trgt.nextMaps trgt) ls')

  {-|
    The 'doSearchAux' performs one step of the algorithm (resolve type 1 and
    type 2 conflicts). It iterates till fixed-point (returns Nothing in case
    of failure).
  -}
  doSearchAux :: State.State -> Maybe [CPointLink.CPointLink]
  doSearchAux state = resolve1 state >>= resolve2 >>= loop
    where
      loop newState
        | links /= newLinks = doSearchAux newState
        | otherwise         = Just newLinks
        where
          links    = State.links state
          newLinks = State.links newState

  {-|

  -}
  resolve1 :: State.State -> Maybe State.State
  resolve1 s = resolve1Aux (State.links s) [] (State.nextMaps s)
    where
      resolve1Aux []         acc m = Just (State.mkState m (L.reverse acc))
      resolve1Aux [l]        acc m = Just (State.mkState m (L.reverse (l:acc)))
      resolve1Aux (l1:l2:ls) acc m
        | CPointLink.biChromaticOrderConflict l1 l2 = update1 (l1:l2:ls)  acc      m
        | otherwise                                 = resolve1Aux (l2:ls) (l1:acc) m

      -- Update CpointLink l2.
      update1 []         _   _ = error "We shouldn't be there"
      update1 [_]        _   _ = error "We shouldn't be there"
      update1 (l1:l2:ls) acc m = ColorMap.updateForNext c trgt2 x m >>= update1Aux
        where
          c      = CPointLink.color l2
          cSrc2  = CPointLink.src  l2
          cTrgt2 = CPointLink.trgt l2
          trgt2  = CPoint.point cTrgt2

          -- New threshold.
          cSrc1 = CPointLink.trgt l1
          src1  = CPoint.point cSrc1
          x     = Point.xCoord src1

          -- resolve CPointLink l2
          updateLink m' = updateLinkAux (ColorMap.next c trgt2 m')
            where
              updateLinkAux Nothing       = error "We shouldn't be there !"
              updateLinkAux (Just trgt2') = CPointLink.mkCPointLinkUnsafe cSrc2 cTrgt2'
                where
                  cTrgt2' = CPoint.mkCPoint trgt2' c

          -- keep on resolving conflict type 1
          update1Aux m' = resolve1Aux (updateLink m':ls) (l1:acc) m'

  {-|

  -}
  resolve2 :: State.State -> Maybe State.State
  resolve2 s = Just s
