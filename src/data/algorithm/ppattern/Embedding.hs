{-|
Module      : Data.Algorithm.PPattern.Embedding
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental
ﬁ8
Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Embedding
(
  -- * The @Embedding@ type
  Embedding(..)
, empty

  -- * Transforming
, embeddingToList

  -- * Querying
, image

  -- * Modifying the embedding
, updateEmbedding
, resolveXEmbedding
, resolveYEmbedding

  -- * Modifying the lastCPoint mapping
, updateLastCPoint
)
where

  import qualified Data.List          as L
  import qualified Data.Tuple         as T
  import qualified Data.Function      as Fun
  import qualified Data.Map.Strict    as Map
  import qualified Data.IntMap.Strict as IntMap
  import qualified Data.Monoid        as Monoid

  import qualified Data.Algorithm.PPattern.CPoint as CPoint
  import qualified Data.Algorithm.PPattern.Color  as Color
  import qualified Data.Algorithm.PPattern.Next   as Next

  data Embedding =  Embedding { embedding  :: Map.Map CPoint.CPoint CPoint.CPoint
                              , lastCPoint :: IntMap.IntMap CPoint.CPoint
                              } deriving (Eq)

  instance Show Embedding where
    show Embedding { embedding=m } = L.intercalate "\n" strList
      where
        asList     = Map.toList m
        sortedList = L.sortBy (compare `Fun.on` (CPoint.xCoord . T.fst)) asList
        strList    = fmap f sortedList
        f (cp1, cp2) = show cp1 `Monoid.mappend` " -> " `Monoid.mappend` show cp2

  {-|
  'empty' constructs an empty embedding.
  -}
  empty :: Embedding
  empty  = Embedding { embedding=Map.empty, lastCPoint=IntMap.empty }

  {-|
    Return the associated embedding.
  -}
  getEmbedding :: Embedding -> Map.Map CPoint.CPoint CPoint.CPoint
  getEmbedding Embedding { embedding=m } = m

  {-|
    Return the associated CPoint mapping.
  -}
  getLastCPoint :: Embedding -> IntMap.IntMap CPoint.CPoint
  getLastCPoint Embedding { lastCPoint=m } = m

  {-|
    Transform an embedding into a list of key/value pairs.
  -}
  embeddingToList :: Embedding -> [(CPoint.CPoint, CPoint.CPoint)]
  embeddingToList = Map.toList . getEmbedding

  {-|
    Update the embedding.
  -}
  updateEmbedding :: CPoint.CPoint -> CPoint.CPoint -> Embedding -> Embedding
  updateEmbedding cp cp' e = e { embedding=m' }
    where
      m  = getEmbedding e
      m' = case Map.lookup cp m of
            Nothing -> Map.insert cp cp' m
            Just _  -> Map.update (\_ -> Just cp') cp m

  {-|
  -}
  image :: CPoint.CPoint -> Embedding -> Maybe CPoint.CPoint
  image cp Embedding { embedding=m } = Map.lookup cp m

  {-|
  -}
  resolveXEmbedding :: CPoint.CPoint -> Int -> Next.Next -> Embedding -> Maybe Embedding
  resolveXEmbedding cp thrshld n e = image cp e                    >>=
                                     Next.jmpThresholdXQ thrshld n >>=
                                     resolveAux cp n e

  {-|
  -}
  resolveYEmbedding :: CPoint.CPoint -> Int -> Next.Next -> Embedding -> Maybe Embedding
  resolveYEmbedding cp thrshld n e = image cp e                    >>=
                                     Next.jmpThresholdYQ thrshld n >>=
                                     resolveAux cp n e

  -- {-|
  -- -}
  -- resolveAux :: CPoint.CPoint -> Next.Next -> Embedding -> Int -> Maybe Embedding
  -- resolveAux cp1 n e k = image cp1 e     >>=
  --                        Next.nextQK k n >>=
  --                        aux
  --   where
  --     aux :: CPoint.CPoint -> Maybe Embedding
  --     aux cp2 = case Next.nextP cp1 n of
  --                  Nothing   -> Just e'
  --                  Just cp1' -> resolveAux cp1' n e' k
  --       where
  --         e' = updateEmbedding cp1 cp2 e

  {-|
  -}
  resolveAux :: CPoint.CPoint -> Next.Next -> Embedding -> Int -> Maybe Embedding
  resolveAux cp n e k = image cp e      >>=
                        Next.nextQK k n >>=
                        resolveAux' cp n e k

  -- resolveAux first auxiliary function. Update the embedding cp -> cp' and call
  -- resolveAux for the next colored point.
  resolveAux' :: CPoint.CPoint -> Next.Next -> Embedding -> Int -> CPoint.CPoint -> Maybe Embedding
  resolveAux' cp n e k cp' = case Next.nextP cp n of
                               Nothing   -> resolveAux'' cp e'
                               Just cp'' -> resolveAux cp'' n e' k
        where
          e' = updateEmbedding cp cp' e

  -- resolveAux second auxiliary function. Update lastCPoint mapping.
  resolveAux'' :: CPoint.CPoint -> Embedding -> Maybe Embedding
  resolveAux'' cp e = Just e'
    where
      c  = CPoint.color cp
      e' = updateLastCPoint c cp e

  -- Update the lastCPoint mapping.
  updateLastCPoint :: Color.Color -> CPoint.CPoint -> Embedding -> Embedding
  updateLastCPoint c cp e = e { lastCPoint=m' }
    where
      m  = getLastCPoint e
      m' = case IntMap.lookup c m of
            Nothing -> IntMap.insert c cp m
            Just _  -> IntMap.update (\_ -> Just cp) c m
