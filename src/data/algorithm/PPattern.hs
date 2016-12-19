{-|
Module      : Data.Algorithm.PPattern
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern
(
  search
, resolve1
)
where

  import qualified Data.List         as L
  import qualified Data.Map.Strict   as Map
  import qualified Data.Tuple.HT     as THT
  import qualified Data.Array        as Array
  import qualified Data.Function     as Fun

  import qualified Data.Algorithm.PPattern.Permutation.Splitting as Splitting
  import qualified Data.Algorithm.PPattern.Point                 as Point
  import qualified Data.Algorithm.PPattern.CPoint                as CPoint
  import qualified Data.Algorithm.PPattern.CPointLink            as CPointLink
  import qualified Data.Algorithm.PPattern.CPointMapping         as CPointMapping
  import qualified Data.Algorithm.PPattern.Stack                 as Stack

  type PointPointMap = Map.Map Point.Point Point.Point

  type CPointPointMap = Map.Map C PointPointMap

  {-|
    'mkNextIncreasing'
  -}
  mkNextIncreasing :: [Point.Point] -> PointPointMap
  mkNextIncreasing []     = Map.empty
  mkNextIncreasing (p:ps) = mkNextIncreasingAux ps s m
    where
      s = Stack.push Stack.empty p
      m = Map.empty

  mkNextIncreasingAux :: [Point.Point] -> Stack Point.Point -> PointPointMap -> PointPointMap
  mkNextIncreasingAux []     _            m = m
  mkNextIncreasingAux (p:ps) s@(Stack []) m = mkNextIncreasingAux ps (Stack.push s p) m
  mkNextIncreasingAux ps     s            m = aux ps s m
    where
      aux ps     s@(Stack [])       m = mkNextIncreasingAux ps s m
      aux (p:ps) s@(Stack (p':ps')) m
        | x' < x    = aux (p:ps) (Stack.pop s) (Map.insert p' p m)
        | otherwise = mkNextIncreasingAux ps (Stack.push s p) m
        where
          x  = Point.xCoord p
          x' = Point.xCoord p'

  {-|
    'mkNextIncreasings' takes a list of colored points. It returns a map that
    associates to each distinct color the next increassing map to this color.
  -}
  mkNextIncreasings :: [CPoint.CPoint] -> CPointPointMap
  mkNextIncreasings cps = mkNextIncreasingsAux cps cs m
    where
      cs = L.nub $ L.filter color cps
      m  = Map.empty

  mkNextIncreasingsAux :: [CPoint.PointC] -> [CPoint.C] -> PointMap -> CPointPointMap
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
  mkCPoints :: Permutation -> Map.Map Int CPoint.C -> [CPoint.CPoint]
  mkCPoints (Permutation xs) m = L.map (THT.uncurry3 CPoint.mkCPoint') [] t3s
    where
      cs  = F.foldr (\x -> fromJust (Map.lookup x m)) xs
      t3s = L.zip3 [1..] xs cs

  {-|
    'leftmostCPointMapping cp1s cp2s' return the leftmost color friendly mapping
    from 'cp1s' to 'cp2s'.
  -}
  leftmostCPointMapping :: [CPoint.CPoint c] -> [CPoint.CPoint c] -> Maybe [CPointLink c]
  leftmostCPointMapping [] []  = Just []
  leftmostCPointMapping [] _   = Just []
  leftmostCPointMapping _  []  = Nothing
  leftmostCPointMapping (cp1:cp1s) (cp2:cp2s)
    | c1 = c2   = mkCPointLink cp1 cp2 >>= \cpl -> fmap (cpl:) (aux cp1s cp2s)
    | otherwise = leftmostCPointMapping (cp1:cp1s) cp2s
    where
      c1 = CPoint.color cp1
      c2 = CPoint.color cp2

  {-|
    'mapFrompartition' transform an integer partition into a map object that
    associates to each each element a color.
  -}
  mapFrompartition :: [Permutation] -> Map.Map Int CPoint.C
  mapFrompartition = Map.fromList . F.concatMap f . flip zip [1..] . fmap Permutation.toList
    where
      f (xs, i) = L.zip xs (L.repeat i)

  {-|

  -}
  mkSources ::Permutation -> Int -> [([CPoint], CPointPointMap)]
  mkSources p = aux $ Splitting.partitionsIncreasings p
    where
      aux []                     = []
      aux (partition:partitions) = (cps, mkNextIncreasings cps):aux partitions
        where
          m   = mapFrompartition partition
          cps = mkCPoints p m

  {-|

  -}
  mkTarget :: Permutation -> ([CPoint], CPointPointMap)
  mkTarget p = (cps, mkNextIncreasings cps)
    where
      partition = Splitting.greedyPartitionIncreasings1 p
      m         = mapFrompartition partition
      cps       = mkCPoints p m

  {-|
    The 'search' function takes two permutations 'p1' and 'p2', and it return
    (if possible) an order-isomorphic occurrence of 'p1' in 'p2'.
  -}
  search :: Permutation -> Permutation -> Maybe Permutation
  search p1 p2 = searchAux cps1 m1 cyss
    where
      cp1s  = mkCPoint p1
      k    = nbColors xs'
      cyss = colors ys k

  {-|
    The 'square' function squares an integer.
    It takes one argument, of type 'Int'.
  -}
  searchAux :: [CPoint c] -> [[CPoint Int Int]]
  searchAux _   [] = Nothing
  searchAux cxs (cys:cyss) = case doSearch cxs cys of
    Nothing -> searchAux cxs cyss
    Just xs -> Just xs

  doSearch :: [CPoint Int Int] -> [[CPoint Int Int]] -> Maybe [Int]
  doSearch cxs cys = Nothing

  search p q = leftmostCPointMapping p q >>= aux
    where
      aux cm = aux' (resolve1 p q cm >>= resolve2 p q)
        where
          aux' Nothing  = Nothing
          aux (Just cm')
            | cm /= cm' = aux cm' -- keep on resolving conflicts
            | otherwise = cm'     -- conflict-free mapping, we're done


  {-|
    The 'square' function squares an integer.
    It takes one argument, of type 'Int'.
  -}
  -- resolve1 :: CPointMapping a c -> Maybe (CPointMapping a c)
  -- resolve1 []        = Just []
  -- resolve1 [l]       = Just [l]
  -- resolve1 (l:l':ls)
  --   | conflict l l' = fmap (l:) (resolve1 (update1 l':ls))
  --   | otherwise     = fmap (l:) (resolve1 l':ls)
  --   where
  --     bichromatic l l'   = colour l /= colour l'
  --     orderConflict l l' = (sPoint l) `southEastDomination` (tPoint l')
  --     confict l l'       = bichromatic l l' && orderConflict l l'
