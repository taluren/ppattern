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
where

  import qualified Data.List       as L
  import qualified Data.Map.Strict as Map
  import qualified Data.Tuple.HT   as THT
  import qualified Data.Foldable   as Fold
  import Data.Maybe

  import qualified Data.Algorithm.PPattern.Types        as T
  import qualified Data.Algorithm.PPattern.Perm         as Perm
  import qualified Data.Algorithm.PPattern.IntPartition as IntPartition
  import qualified Data.Algorithm.PPattern.CPoint       as CPoint
  import qualified Data.Algorithm.PPattern.Color        as Color
  import qualified Data.Algorithm.PPattern.CPLink       as CPLink
  import qualified Data.Algorithm.PPattern.Strategy     as Strategy
  import qualified Data.Algorithm.PPattern.Next         as Next
  import qualified Data.Algorithm.PPattern.Embedding    as Embedding

  {-|
    Construct a list of colored points from a permutation and a color mapping.
  -}
  mkCPoints :: Perm.Perm -> Map.Map T.T Color.Color -> [CPoint.CPoint]
  mkCPoints (Perm.Perm xs) m = L.map (THT.uncurry3 CPoint.mkCPoint) t3s
    where
      cs  = Fold.foldr (\x acc -> fromJust (Map.lookup x m):acc) [] xs
      t3s = L.zip3 [1..] xs cs

  {-|
    Construct the leftmost color-friendly mapping from two lists of colored
    points.
  -}
  leftmostEmbedding :: [CPoint.CPoint] -> [CPoint.CPoint] -> Maybe Embedding.Embedding
  leftmostEmbedding cp1s cp2s = Embedding.fromList <$> leftmostEmbeddingAux cp1s cp2s

  leftmostEmbeddingAux :: [CPoint.CPoint] -> [CPoint.CPoint] -> Maybe [(CPoint.CPoint, CPoint.CPoint)]
  leftmostEmbeddingAux [] []  = Just []
  leftmostEmbeddingAux [] _   = Just []
  leftmostEmbeddingAux _  []  = Nothing
  leftmostEmbeddingAux (cp1:cp1s) (cp2:cp2s)
    | c1 == c2  = fmap ((cp1, cp2):) (leftmostEmbeddingAux cp1s cp2s)
    | otherwise = leftmostEmbeddingAux (cp1:cp1s) cp2s
    where
      c1 = CPoint.color cp1
      c2 = CPoint.color cp2

  {-|

  -}
  mapFromPartition :: [Perm.Perm] -> Map.Map T.T Color.Color
  mapFromPartition = Map.fromList . Fold.concatMap f . flip zip ([1..] :: [Color.Color]) . fmap Perm.toList
    where
      -- use colors 1, 2, ...
      colors = L.repeat

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
    'mkPs p k' returns all 'k'-coloring of permutation 'p', where each coloring
    induces an increasing subsequence.
  -}
  mkPs :: Perm.Perm -> Int -> IntPartition.IntPartition -> [[CPoint.CPoint]]
  mkPs p k intPartitionQ
    | k > Perm.size p = mkPs p (k-1) intPartitionQ
    | otherwise       = aux (Perm.partitionsIncreasings p k)
    where
      -- Add every compatible partition.
      aux [] = []
      aux (partitionP:partitionsP)
        | IntPartition.compatible intPartitionP intPartitionQ = cps:aux partitionsP
        | otherwise                                           = aux partitionsP
        where
          intPartitionP = toIntPartition partitionP
          cps = mkCPoints p (mapFromPartition partitionP)

  {-|

  -}
  mkQ :: Perm.Perm -> ([CPoint.CPoint], IntPartition.IntPartition)
  mkQ q = (cps, toIntPartition partition)
    where
      partition = Perm.greedyPartitionIncreasings1 q
      cps       = mkCPoints q (mapFromPartition partition)

  {-|
    The 'search' function takes two permutations 'p' and 'q', and it returns
    (if possible) an order-isomorphic occurrence of 'p' in 'q'.
  -}
  search :: Perm.Perm -> Perm.Perm -> Strategy.Strategy -> Maybe Embedding.Embedding
  search p q = searchAux cpssP cpsQ n
    where
      -- make target structure
      (cpsQ, intPartitionQ) = mkQ q

      -- make all source structures
      cpssP = mkPs p (CPoint.nbColors cpsQ) intPartitionQ

      -- make next function for permutation q
      n = Next.mkQ' cpsQ

  searchAux :: [[CPoint.CPoint]] -> [CPoint.CPoint] -> Next.Next -> Strategy.Strategy -> Maybe Embedding.Embedding
  searchAux []           _    _ _ = Nothing
  searchAux (cpsP:cpssP) cpsQ n s = case doSearch cpsP cpsQ n s of
                                      Nothing -> searchAux cpssP cpsQ n s
                                      Just e  -> Just e

  {-|
    Perform the search for a given coloring of the source Perm.
  -}
  doSearch :: [CPoint.CPoint] -> [CPoint.CPoint] -> Next.Next -> Strategy.Strategy -> Maybe Embedding.Embedding
  doSearch cpsP cpsQ n s = leftmostEmbedding cpsP cpsQ >>= doSearchAux n s

  doSearchAux :: Next.Next -> Strategy.Strategy -> Embedding.Embedding -> Maybe Embedding.Embedding
  doSearchAux n s e = resolveConflict n s e >>= loop
    where
      loop e'
        | e /= e'   = doSearchAux n s e'
        | otherwise = Just e'

  {-|
  -}
  resolveConflict :: Next.Next -> Strategy.Strategy -> Embedding.Embedding -> Maybe Embedding.Embedding
  resolveConflict n s e = resolveConflictAux (s e) n s e

  {-|
  -}
  resolveConflictAux :: [(CPLink.CPLink, CPLink.CPLink)] -> Next.Next -> Strategy.Strategy -> Embedding.Embedding -> Maybe Embedding.Embedding
  resolveConflictAux []                     _ _ e = Just e
  resolveConflictAux ((lnk1, lnk2):lnkLnks) n s e
    | CPLink.orderConflict lnk1 lnk2 = resolveOrderConflict lnk1 lnk2 n s e
    | CPLink.orderConflict lnk2 lnk1 = resolveOrderConflict lnk2 lnk1 n s e
    | CPLink.valueConflict lnk1 lnk2 = resolveValueConflict lnk1 lnk2 n s e
    | CPLink.valueConflict lnk2 lnk1 = resolveValueConflict lnk2 lnk1 n s e
    | otherwise                      = resolveConflictAux lnkLnks n s e

  {-|
  -}
  resolveOrderConflict :: CPLink.CPLink -> CPLink.CPLink -> Next.Next -> Strategy.Strategy -> Embedding.Embedding -> Maybe Embedding.Embedding
  resolveOrderConflict lnk1 lnk2 n s e = Embedding.resolveX cp thrshld n e >>= resolveConflict n s
    where
      cp      = CPLink.cpP lnk2
      thrshld = CPoint.xCoord $ CPLink.cpQ lnk1

  {-|
  -}
  resolveValueConflict :: CPLink.CPLink -> CPLink.CPLink -> Next.Next -> Strategy.Strategy -> Embedding.Embedding -> Maybe Embedding.Embedding
  resolveValueConflict lnk1 lnk2 n s e = Embedding.resolveX cp thrshld n e >>= resolveConflict n s
    where
      cp      = CPLink.cpP lnk2
      thrshld = CPoint.yCoord $ CPLink.cpQ lnk1
