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

  -- to be removed from export
, leftmostCPMapping
)
where

  import qualified Data.List          as L
  import qualified Data.Map.Strict    as Map
  import qualified Data.IntMap.Strict as IntMap
  import qualified Data.Tuple.HT      as THT
  import qualified Data.Foldable      as Fold
  import Data.Maybe

  import qualified Data.Algorithm.PPattern.Permutation  as Permutation
  import qualified Data.Algorithm.PPattern.IntPartition as IntPartition
  import qualified Data.Algorithm.PPattern.Point        as Point
  import qualified Data.Algorithm.PPattern.CPoint       as CPoint
  import qualified Data.Algorithm.PPattern.Color        as Color
  import qualified Data.Algorithm.PPattern.Trgt         as Trgt
  import qualified Data.Algorithm.PPattern.PMap         as PMap
  import qualified Data.Algorithm.PPattern.CMap         as CMap
  import qualified Data.Algorithm.PPattern.CMaps        as CMaps
  import qualified Data.Algorithm.PPattern.CPLink       as CPLink
  import qualified Data.Algorithm.PPattern.State        as State
  import qualified Data.Algorithm.PPattern.Stack        as Stack
  import qualified Data.Algorithm.PPattern.Next         as Next

  {-|
    The 'mkCPoints' function takes a permutation and a color mapping given
    in the form of a map, and it return a list of CPoint.
  -}
  mkCPoints :: Perm.Perm -> IntMap.IntMap Color.Color -> [CPoint.CPoint]
  mkCPoints (Perm.Perm xs) m = L.map (THT.uncurry3 CPoint.mkCPoint') t3s
    where
      cs  = Fold.foldr (\x acc -> (fromJust (Map.lookup x m)):acc) [] xs
      t3s = L.zip3 [1..] xs cs

  {-|
    'leftmostCPMapping cp1s cp2s' return the leftmost color friendly mapping
    from 'cp1s' to 'cp2s'.
  -}
  leftmostCPMapping :: [CPoint.CPoint] -> [CPoint.CPoint] -> Maybe [CPLink.CPLink]
  leftmostCPMapping [] []  = Just []
  leftmostCPMapping [] _   = Just []
  leftmostCPMapping _  []  = Nothing
  leftmostCPMapping (cp1:cp1s) (cp2:cp2s)
    | c1 == c2   = CPLink.mkCPLink cp1 cp2 >>= \cpl -> fmap (cpl:) (leftmostCPMapping cp1s cp2s)
    | otherwise = leftmostCPMapping (cp1:cp1s) cp2s
    where
      c1 = CPoint.color cp1
      c2 = CPoint.color cp2

  {-|
    'mapFromPartition' transform an integer partition into a map object that
    associates to each each element a color.
  -}
  mapFromPartition :: [Perm.Perm] -> IntMap.IntMap Color.Color
  mapFromPartition = Map.fromList . Fold.concatMap f . flip zip [1..] . fmap Perm.toList
    where
      colors i  = L.repeat i

      f (xs, i) = L.zip xs $ colors i

  {-|
    'toIntPartition ps' returns the lengths of all partition.
  -}
  toIntPartition :: [Perm.Perm] -> IntPartition.IntPartition
  toIntPartition ps = IntPartition.mkIntPartition ls'
    where
      ls  = fmap Perm.size ps
      ls' = L.sortBy (flip compare) ls

  {-|
    'mkSrc p k' returns all 'k'-coloring of permutation 'p', where each coloring
    is an increasing subsequence.
  -}
  mkSrc :: Perm.Perm -> Int -> IntPartition.IntPartition -> [[CPoint.CPoint]]
  mkSrc p k ip = aux pis
    where
      pis = Perm.partitionsIncreasings p k

      -- Add every compatible partition.
      aux []= []
      aux (partition:partitions)
        | IntPartition.compatible ip2 ip = cps:aux partitions
        | otherwise                      = aux partitions
        where
          ip2 = toIntPartition partition
          cps = mkCPoints p (mapFromPartition partition)

  {-|
    the 'mkTrgt' function is in charge of finding a k-increasing-coloring
    of a given Perm. It returns the next function for each color in the
    form of a map.
  -}
  mkTrgt :: Perm.Perm -> (Trgt.Trgt, IntPartition.IntPartition)
  mkTrgt p = (Trgt.mkTrgt cps xcm ycm, toIntPartition partition)
    where
      partition    = Perm.greedyPartitionIncreasings1 p
      cps          = mkCPoints p (mapFromPartition partition)

      -- Next color point map for x-coordinates
      xcm          = Next.mkNextIncreasings cps PMap.XCoord

      -- Next color point map for y-ccordinates
      ycm          = Next.mkNextIncreasings cps PMap.YCoord

  {-|
    The 'search' function takes two permutations 'p' and 'q', and it returns
    (if possible) an order-isomorphic occurrence of 'p' in 'q'.
  -}
  search :: Perm.Perm -> Perm.Perm -> Maybe [CPLink.CPLink]
  search p q = searchAux srcs trgt
    where
      -- make target structure
      (trgt, ip) = mkTrgt q

      -- make all source structures
      srcs = mkSrc p (Trgt.nbColors trgt) ip

  searchAux :: [[CPoint.CPoint]] -> Trgt.Trgt -> Maybe [CPLink.CPLink]
  searchAux []               _    = Nothing
  searchAux (srcCps:srcCpss) trgt = case doSearch srcCps trgt of
                                      Nothing -> searchAux srcCpss trgt
                                      Just q  -> Just q

  {-|
    Perform the search for a given coloring of the source permutation.
  -}
  doSearch :: [CPoint.CPoint] -> Trgt.Trgt -> Maybe [CPLink.CPLink]
  doSearch srcPoints trgt = lnks >>= mkState >>= doSearchAux
    where
      lnks          = leftmostCPMapping srcPoints (Trgt.cPoints trgt)
      xcm           = Trgt.xCMap trgt
      ycm           = Trgt.yCMap trgt
      cms           = CMaps.mkCMaps xcm ycm
      mkState lnks' = Just (State.mkState cms lnks')

  doSearchAux :: State.State -> Maybe [CPLink.CPLink]
  doSearchAux state = resolve1 state >>= resolve2 >>= loop
    where
      loop state'
        | lnks /= lnks' = doSearchAux state'
        | otherwise     = Just lkns'
        where
          lnks  = State.links state
          lnks' = State.links state'

  {-|
    Resolve type 1 conflicts.
  -}
  resolve1 :: State.State -> Maybe State.State
  resolve1 state = resolve1Aux (State.links state) [] state

  resolve1Aux []               acc state = Just state'
  resolve1Aux [lnk]            acc state = Just state''
  resolve1Aux (lnk1:lnk2:lnks) acc state
    | conflict lnk1 lnk2 = update1 (lnk1:lnk2:lnks) acc state
    | otherwise          = resolve1Aux (lkn2:lnks) (lnk1:acc) state
    where
      -- Two links with the same color but reverse x-coordinate order
      confict = CPLink.monoChromaticOrderConflict

      -- Done, propagate a new state
      state' = State.mkState (L.reverse acc) cms
        where
          cms = CMaps.mkCMaps (State.xCMaps state) (State.yCMap state)

      -- Done, propagate a new state
      state'' = State.mkState (L.reverse (lnk:acc)) cms
        where
          cms = CMaps.mkCMaps (State.xCMaps state) (State.yCMap state)


      -- Update link l2.
      update1 []         _   _     = error "We shouldn't be there"
      update1 [_]        _   _     = error "We shouldn't be there"
      update1 (l1:l2:ls) acc state = CMap.updateForNext c trgt2 x m >>= update1Aux
        where
          c      = CPLink.color l2
          cSrc2  = CPLink.src  l2
          cTrgt2 = CPLink.trgt l2
          trgt2  = CPoint.point cTrgt2

          -- New threshold.
          cSrc1 = CPLink.trgt l1
          src1  = CPoint.point cSrc1
          x     = Point.xCoord src1

          -- resolve CPLink l2
          updateLink m' = updateLinkAux (CMap.next c trgt2 m')
            where
              updateLinkAux Nothing       = error "We shouldn't be there !"
              updateLinkAux (Just trgt2') = CPLink.mkCPLinkUnsafe cSrc2 cTrgt2'
                where
                  cTrgt2' = CPoint.mkCPoint trgt2' c

          -- keep on resolving conflict type 1
          update1Aux m' = resolve1Aux (updateLink m':ls) (l1:acc) m'

  {-|

  -}
  resolve2 :: State.State -> Maybe State.State
  resolve2 s = Just s
