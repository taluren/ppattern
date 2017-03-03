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
, searchAux
, doSearch
, doSearchAux
)
where

  -- import qualified Data.List          as L
  -- import qualified Data.IntMap.Strict as IntMap

  import qualified Data.Algorithm.PPattern.Perm      as Perm
  -- import qualified Data.Algorithm.PPattern.Color     as Color
  import qualified Data.Algorithm.PPattern.CPoint    as CPoint
  import qualified Data.Algorithm.PPattern.CPLink    as CPLink
  import qualified Data.Algorithm.PPattern.Strategy  as Strategy
  import qualified Data.Algorithm.PPattern.Next      as Next
  import qualified Data.Algorithm.PPattern.Embedding as Embedding
  import qualified Data.Algorithm.PPattern.Strategy  as Strategy


  -- Make an initial list of colored points. Each element from the longest
  -- decreasing subsequence is given a distinct colors. All other elements
  -- are given the 'not determined yet' color 0.
  mkCPoints :: [(Int, Int)] -> [Int] -> [Color.Color] -> [(Int, Int, Color.Color)]
  mkCPoints []             _            _         = []
  mkCPoints ((i, x) : ixs) []           refColors = (i, x, 0) : mkCPoints ixs [] refColors
  mkCPoints ((_, _) : _)   _            []        = error "We shouldn't be there"
  mkCPoints ((i, x) : ixs) ys'@(y : ys) refColors'@(c : refColors)
    | x == y    = (i, x, c) : mkCPoints ixs ys  refColors
    | otherwise = (i, x, 0) : mkCPoints ixs ys' refColors'

  {-|
    'increasingPartitions p k' returns all partitions of the permutation 'p' into
    'k' increasing subsequences.
  -}
  increasingPartitions :: Perm -> Int -> [[CPoint.CPoint]]
  increasingPartitions p k
    | l > k     = []
    | otherwise = Foldable.concat [increasingPartitionsAux cps cs prevMap nextMap
                                  | refColors     <- [1..k] `Combi.choose` l
                                  -- , permRefColors <- L.permutations refColors
                                  , let cps     = mkCPoints indexed decreasing refColors
                                  , let prevMap = IntMap.empty
                                  , let nextMap = IntMap.fromList $ L.zip refColors decreasing]
    where
      -- A longest decreasing subsequence of p.
      decreasing = longestDecreasing p

      -- The length oa longest decreasing subsequence of p.
      l = L.length decreasing

      -- The list of available colors.
      cs = [1..k]

      -- Index permutation p.
      indexed = index p

  increasingPartitionsAux :: [(Int, Int, Color.Color)] -> [Color.Color] -> IntMap.IntMap Int -> IntMap.IntMap Int -> [[CPoint.CPoint]]
  increasingPartitionsAux []                 _  _       _       = [[]]
  increasingPartitionsAux ((i, x, 0) : ixcs) cs prevMap nextMap = increasingPartitionsAux1  (i, x)    ixcs cs prevMap nextMap
  increasingPartitionsAux ((i, x, c) : ixcs) cs prevMap nextMap = increasingPartitionsAux2 (i, x, c) ixcs cs prevMap nextMap

  {-|
    increasingPartitionsAux for color-0 points (those point that are not part of
    the selected longest decreasing subsequence).
  -}
  increasingPartitionsAux1 :: (Int, Int)-> [(Int, Int, Color.Color)] -> [Color.Color] -> IntMap.IntMap Int -> IntMap.IntMap Int -> [[CPoint.CPoint]]
  increasingPartitionsAux1 (i, x) ixcs cs prevMap nextMap = Foldable.concat cpsss
    where
      cpsss = [fmap (cp :) cpss | c <- cs
                                , agreeWithPrevElement x c prevMap
                                , agreeWithNextElement x c nextMap
                                , let cp       = CPoint.mkCPoint i x c
                                , let prevMap' = updatePrevMap x c prevMap
                                , let nextMap' = updateNextMap x c nextMap
                                , let cpss     = increasingPartitionsAux ixcs cs prevMap' nextMap']

  {-|
    increasingPartitionsAux for colored points (those point that are part of
    the selected longest decreasing subsequence).
  -}
  increasingPartitionsAux2 :: (Int, Int, IntMap.Key) -> [(Int, Int, Color.Color)] -> [Color.Color] -> IntMap.IntMap Int -> IntMap.IntMap Int  -> [[CPoint.CPoint]]
  increasingPartitionsAux2 (i, x, c) ixcs cs prevMap nextMap = fmap (cp :) cpss
    where
      -- Nex color point
      cp  = CPoint.mkCPoint i x c

      -- This color point in now a left constraint
      prevMap' = updatePrevMap x c prevMap

      -- This color point in now a left constraint
      nextMap' = updateNextMap x c nextMap

      -- recursively compute the remaining colored points
      cpss = increasingPartitionsAux ixcs cs prevMap' nextMap'


  agreeWithPrevElement :: Color.Color -> IntMap.Key -> IntMap.IntMap Int -> Bool
  agreeWithPrevElement x c m = case IntMap.lookup c m of
                                 Nothing -> True
                                 Just x' -> x' < x

  agreeWithNextElData.Algorithm.PPattern.Embedding as Embeddingement :: Color.Color -> IntMap.Key -> IntMap.IntMap Int -> Bool
  agreeWithNextElement x c m = case IntMap.lookup c m of
                                 Nothing -> True
                                 Just x' -> x < x'

  updatePrevMap :: Color.Color -> IntMap.Key -> IntMap.IntMap Int -> IntMap.IntMap Int
  updatePrevMap x c m = updatePrevMapAux x 1 c m

  updatePrevMapAux :: Int -> IntMap.Key -> IntMap.Key -> IntMap.IntMap Int -> IntMap.IntMap Int
  updatePrevMapAux x c c' m
    | c > c'    = m
    | otherwise = updatePrevMapAux  x (c+1) c' m'
      where
        m' = case IntMap.lookup c m of
               Nothing -> IntMap.insert c x m
               Just _  -> IntMap.update (\y -> Just (max x y)) c m

  updateNextMap :: Color.Color -> IntMap.Key -> IntMap.IntMap Int -> IntMap.IntMap Int
  updateNextMap x c m = case IntMap.lookup c m of
                          Nothing       -> m
                          Just x'
                            | x < x'    -> m
                            | x == x'   -> IntMap.delete c m
                            | otherwise -> error "We shouldn't be there 2" -- make ghc -Werror happy

-- 'mkPs p k' returns all 'k'-coloring of permutation 'p', where each coloring
  -- induces an increasing subsequence.
  mkPs :: Perm.Perm -> Int -> [[CPoint.CPoint]]
  mkPs = Perm.increasingPartitions

  -- Partition a permutation into a minimum number of increasing subsequences.
  -- The result is given in the form of a list of colored points (i.e., each color
  -- induces an increasing subsequence).
  mkQ :: Perm.Perm -> [CPoint.CPoint]
  mkQ = Perm.increasingPartition

  {-|
    The 'search' function takes two permutations 'p' and 'q', and it returns
    (if possible) an order-isomorphic occurrence of 'p' in 'q'.
  -}
  search :: Perm.Perm -> Perm.Perm -> Strategy.Strategy -> Maybe State.State
  search p q strtgy
    | m > n     = Nothing
    | l > k     = Nothing
    | otherwise = searchAux p s
    where
      m = Perm.size p
      n = Perm.size q

      l = Perm.longestDecreasingLength p
      k = Perm.longestDecreasingLength q

      s = mkState q strtgy

  -- Permutation pattern matching for all possible coloouring of the source
  -- permutation.
  searchAux :: [[CPoint.CPoint]] -> [CPoint.CPoint] -> Next.Next -> Strategy.Strategy -> Maybe Embedding.Embedding
  searchAux []           _    _ _ = Nothing
  searchAux (cpPs:cpPss) cpQs n s = case doSearch cpPs cpQs (Next.mkP cpPs n) s of
                                        Nothing -> searchAux cpPss cpQs n s
                                        Just e  -> Just e

  -- Perform the search for a given coloring of the source Perm.
  doSearch :: [CPoint.CPoint] -> [CPoint.CPoint] -> Next.Next -> Strategy.Strategy -> Maybe Embedding.Embedding
  doSearch []   _    _ _ = Nothing
  doSearch _    []   _ _ = Nothing
  doSearch cpPs cpQs n s = leftmostEmbedding cpPs cpQs >>= doSearchAux n s

  doSearchAux :: Next.Next -> Strategy.Strategy -> Embedding.Embedding -> Maybe Embedding.Embedding
  doSearchAux n s e = resolveConflict n s e >>= loop
    where
      loop e'
        | e == e'   = Just e
        | otherwise = doSearchAux n s e'

  -- Prepare to resolve conflicts. The function first uses its strategy to order
  -- pairs of links.
  resolveConflict :: Next.Next -> Strategy.Strategy -> Embedding.Embedding -> Maybe Embedding.Embedding
  resolveConflict n s e = resolveConflictAux (s e) n s e

  -- Resolve one conflict.
  resolveConflictAux :: [(CPLink.CPLink, CPLink.CPLink)] -> Next.Next -> Strategy.Strategy -> Embedding.Embedding -> Maybe Embedding.Embedding
  resolveConflictAux []                     _ _ e = Just e
  resolveConflictAux ((lnk1, lnk2):lnkLnks) n s e
    | CPLink.orderConflict lnk1 lnk2 = resolveOrderConflict lnk1 lnk2 n s e
    | CPLink.orderConflict lnk2 lnk1 = resolveOrderConflict lnk2 lnk1 n s e
    | CPLink.valueConflict lnk1 lnk2 = resolveValueConflict lnk1 lnk2 n s e
    | CPLink.valueConflict lnk2 lnk1 = resolveValueConflict lnk2 lnk1 n s e
    | otherwise                      = resolveConflictAux lnkLnks n s e

  -- Resolve (if possible) some order-conflict.
  resolveOrderConflict :: CPLink.CPLink -> CPLink.CPLink -> Next.Next -> Strategy.Strategy -> Embedding.Embedding -> Maybe Embedding.Embedding
  resolveOrderConflict lnk1 lnk2 n s e = Embedding.resolveX cpP2 thrshld n e >>= resolveConflict n s
    where
      cpP2    = CPLink.cpP lnk2
      cpQ1    = CPLink.cpQ lnk1
      thrshld = CPoint.xCoord cpQ1

  -- Resolve (if possible) some value conflict.
  resolveValueConflict :: CPLink.CPLink -> CPLink.CPLink -> Next.Next -> Strategy.Strategy -> Embedding.Embedding -> Maybe Embedding.Embedding
  resolveValueConflict lnk1 lnk2 n s e = Embedding.resolveY cpP2 thrshld n e >>= resolveConflict n s
    where
      cpP2    = CPLink.cpP lnk2
      cpQ1    = CPLink.cpQ lnk1
      thrshld = CPoint.yCoord cpQ1
