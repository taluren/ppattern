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

  embeddingCompletion ::[CPoint.CPoint] -> CPoint.CPoint -> [CPoint.CPoint] ->  Embedding.Embedding -> Next.Next -> Maybe Embedding.Embedding
  embeddingCompletion cpPs cpP cpQs e n = 

  -- Construct the leftmost embedding of a list of coloured points into another
  -- list of coloured points.
  leftmostEmbedding :: [CPoint.CPoint] -> [CPoint.CPoint] -> Maybe Embedding.Embedding
  leftmostEmbedding cp1s cp2s = Embedding.fromList <$> leftmostEmbeddingAux cp1s cp2s

  leftmostEmbeddingAux :: [CPoint.CPoint] -> [CPoint.CPoint] -> Maybe [(CPoint.CPoint, CPoint.CPoint)]
  leftmostEmbeddingAux [] []  = Just []
  leftmostEmbeddingAux [] _   = Just []
  leftmostEmbeddingAux _  []  = Nothing
  leftmostEmbeddingAux (cp1 : cp1s) (cp2 : cp2s)
    | c1 == c2  = fmap ((cp1, cp2):) (leftmostEmbeddingAux cp1s cp2s)
    | otherwise = leftmostEmbeddingAux (cp1:cp1s) cp2s
    where
      c1 = CPoint.color cp1
      c2 = CPoint.color cp2

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
  search :: Perm.Perm -> Perm.Perm -> Strategy.Strategy -> Maybe Embedding.Embedding
  search p q s
    | Perm.size p > Perm.size q                                       = Nothing
    | Perm.longestDecreasingLength p > Perm.longestDecreasingLength q = Nothing
    | otherwise                                                       = searchAux cpPss cpQs nQ s
    where
      -- make target colored points list
      cpQs = mkQ q

      -- make all source colored points lists
      cpPss = mkPs p (CPoint.nbColors cpQs)

      -- make next function for permutation q
      nQ = Next.mkQ' cpQs

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
