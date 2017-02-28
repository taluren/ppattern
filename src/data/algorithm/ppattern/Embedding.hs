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

  data Embedding =  Embedding { getMap :: Map.Map CPoint.CPoint CPoint.CPoint } deriving (Eq)

  instance Show Embedding where
    show Embedding { getMap = m } = L.intercalate "\n" strList
      where
        asList      = Map.toList m
        sortedList  = L.sortBy (compare `Fun.on` (CPoint.xCoord . T.fst)) asList
        strList     = fmap f sortedList
        f (cp, cp') = show cp `Monoid.mapêpend` " -> " `Monoid.mappend` show cp'

  {-|
  'empty' constructs an empty embedding.
  -}
  empty :: Embedding
  empty  = Embedding { getMap = Map.empty }

  {-|
    Transform an embedding into a list of key/value pairs.
  -}
  toList :: Embedding -> [(CPoint.CPoint, CPoint.CPoint)]
  toList = Map.toList . getMap

  {-|
    Update the embedding.
  -}
  update :: CPoint.CPoint -> CPoint.CPoint -> Embedding -> Embedding
  update cp cp' e = e { hetMap = m' }
    where
      m  = getMap e
      m' = case Map.lookup cp m of
             Nothing -> Map.insert cp cp' m
             Just _  -> Map.update (\_ -> Just cp') cp m

  {-|
    Return the image of a colored point according to the embedding.
  -}
  image :: CPoint.CPoint -> Embedding -> Maybe CPoint.CPoint
  image cp e = Map.lookup cp . getMap

  {-|
  -}
  xResolve :: CPoint.CPoint -> Int -> Embedding -> Maybe Embedding
  xResolve cp thrshld n e = image cp e                            >>=
                            Next.xJumpThreshold thrshld (pNext s) >>=
                            resolveAux cp n e

  {-|
  -}
  yResolve :: CPoint.CPoint -> Int -> Next.Next -> Embedding -> Maybe Embedding
  yResolve cp thrshld n e = image cp e                    >>=
                            Next.yJumpThreshold thrshld n >>=
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
