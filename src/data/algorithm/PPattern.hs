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
  import qualified Data.Function   as Fun
  import Data.Maybe

  import qualified Data.Algorithm.PPattern.Permutation           as Permutation
  import qualified Data.Algorithm.PPattern.Point                 as Point
  import qualified Data.Algorithm.PPattern.CPoint                as CPoint
  import qualified Data.Algorithm.PPattern.Color                 as Color
  import qualified Data.Algorithm.PPattern.PointToPointMap       as PointToPointMap
  import qualified Data.Algorithm.PPattern.CToPointToPointMapMap as CToPointToPointMapMap
  import qualified Data.Algorithm.PPattern.CPointCPointLink      as CPointCPointLink
  import qualified Data.Algorithm.PPattern.State                 as State
  import qualified Data.Algorithm.PPattern.Struct                as Struct
  import qualified Data.Algorithm.PPattern.Stack                 as Stack

  {-|
    'mkNextIncreasing'
  -}
  mkNextIncreasing :: [Point.Point] -> PointToPointMap.PointToPointMap
  mkNextIncreasing []     = Map.empty
  mkNextIncreasing (p:ps) = mkNextIncreasingAux ps s m
    where
      s = Stack.push Stack.empty p
      m = Map.empty

  mkNextIncreasingAux :: [Point.Point] -> Stack.Stack Point.Point -> PointToPointMap.PointToPointMap -> PointToPointMap.PointToPointMap
  mkNextIncreasingAux []     _                  m = m
  mkNextIncreasingAux (p:ps) s@(Stack.Stack []) m = mkNextIncreasingAux ps (Stack.push s p) m
  mkNextIncreasingAux ps     s                  m = aux ps s m
    where
      aux ps     s@(Stack.Stack [])       m = mkNextIncreasingAux ps s m
      aux (p:ps) s@(Stack.Stack (p':ps')) m
        | x' < x    = aux (p:ps) (Stack.popUnsafe s) (Map.insert p' p m)
        | otherwise = mkNextIncreasingAux ps (Stack.push s p) m
        where
          x  = Point.xCoord p
          x' = Point.xCoord p'

  {-|
    'mkNextIncreasings' takes a list of colored points. It returns a map that
    associates to each distinct color the next increassing map to this color.
  -}
  mkNextIncreasings :: [CPoint.CPoint] -> CToPointToPointMapMap.CToPointToPointMapMap
  mkNextIncreasings cps = mkNextIncreasingsAux cps cs m
    where
      cs = L.nub $ L.map CPoint.color cps
      m  = Map.empty

  {-|

  -}
  mkNextIncreasingsAux :: [CPoint.CPoint] -> [Color.Color] -> CToPointToPointMapMap.CToPointToPointMapMap -> CToPointToPointMapMap.CToPointToPointMapMap
  mkNextIncreasingsAux cps []     m = m
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
  leftmostCPointMapping :: [CPoint.CPoint] -> [CPoint.CPoint] -> Maybe [CPointCPointLink.CPointCPointLink]
  leftmostCPointMapping [] []  = Just []
  leftmostCPointMapping [] _   = Just []
  leftmostCPointMapping _  []  = Nothing
  leftmostCPointMapping (cp1:cp1s) (cp2:cp2s)
    | c1 == c2   = CPointCPointLink.mkCPointCPointLink cp1 cp2 >>= \cpl -> fmap (cpl:) (leftmostCPointMapping cp1s cp2s)
    | otherwise = leftmostCPointMapping (cp1:cp1s) cp2s
    where
      c1 = CPoint.color cp1
      c2 = CPoint.color cp2

  {-|
    'mapFrompartition' transform an integer partition into a map object that
    associates to each each element a color.
  -}
  mapFrompartition :: [Permutation.Permutation] -> Map.Map Permutation.T Color.Color
  mapFrompartition = Map.fromList . Fold.concatMap f . flip zip [1..] . fmap Permutation.toList
    where
      f (xs, i) = L.zip xs (L.repeat i)

  {-|
partitionsIncreasings
  -}
  mkSourceStructs ::Permutation.Permutation -> Int -> [Struct.Struct]
  mkSourceStructs p k = aux $ Permutation.partitionsIncreasings p k
    where
      aux []                     = []
      aux (partition:partitions) = Struct.mkStruct cps (mkNextIncreasings cps):aux partitions
        where
          m   = mapFrompartition partition
          cps = mkCPoints p m

  {-|

  -}
  mkTargetStruct :: Permutation.Permutation -> Struct.Struct
  mkTargetStruct p = Struct.mkStruct cps (mkNextIncreasings cps)
    where
      partition = Permutation.greedyPartitionIncreasings1 p
      m         = mapFrompartition partition
      cps       = mkCPoints p m

  -- {-|
  --   The 'parSearch'
  -- -}
  -- parSearch :: Permutation.Permutation -> Permutation.Permutation -> Int -> Maybe State.State
  -- parSearch p1 p2 = aux (mkSourceStructs p1) (mkTargetStruct p2)
  --   where
  --     aux []                     _          = Nothing
  --     aux (src:srcs) trgt = aux $ doSearch src trgt
  --       where
  --         aux Nothing = aux srcs trgt
  --         aux Just q  = Just q

  {-|
    The 'search' function takes two permutations 'p' and 'q', and it returns
    (if possible) an order-isomorphic occurrence of 'p' in 'q'.
  -}
  search :: Permutation.Permutation -> Permutation.Permutation -> Maybe State.State
  search p q = aux srcStructs trgtStruct
    where
      --
      trgtStruct = mkTargetStruct q

      srcStructs = mkSourceStructs p (Struct.nbColors trgtStruct)

      aux []          _   = Nothing
      aux (src:srcs) trgt = case doSearch src trgt of
        Nothing -> aux srcs trgt
        Just q  -> Just q

  {-|

  -}
  doSearch :: Struct.Struct -> Struct.Struct -> Maybe State.State
  doSearch src trgt = m >>= State.mkState >>= doSearchAux
    where
      srcCPoints  = Struct.cPoints src
      trgtCPoints = Struct.cPoints trgt
      m           = leftmostCPointMapping srcCPoints trgtCPoints

  {-|

  -}
  doSearchAux :: State.State -> Maybe State.State
  doSearchAux state = return state >>= resolve1  >>= resolve2 >>= loop
    where
      loop state'
        | links /= links' = doSearchAux state'
        | otherwise       = Just (getOccurrence links)
        where
          links  = State.links state
          links' = State.links state'
  {-|

  -}
  getOccurrence :: [CPointCPointLink.CPointCPointLink] -> Permutation.Permutation
  getOccurrence = Permutation.fromListUnsafe . fmap f
    where
      f = Point.yCoord . CPoint.point . CPointCPointLink.trgt

  {-|

  -}
  resolve1 :: State.State -> Maybe State.State
  resolve1 s = aux (State.links s) [] (State.src s) (State.trgt s)
    where
      aux []         acc src trgt = Just (State.mkState (L.reverse acc)   src trgt)
      aux [l]        acc src trgt = Just (State.mkState (L.reverse l:acc) src trgt)
      aux (l1:l2:ls) acc src trgt
        | CPointCPointLink.biChromaticOrderConflict l1 l2 = update1 (l1:l2:ls) acc src trgt
        | otherwise                                       = aux (l2:ls) (l1:acc) src trgt

  update1 _ _ _ _ = Nothing

  resolve2 :: State.State -> Maybe State.State
  resolve2 _ = Nothing
