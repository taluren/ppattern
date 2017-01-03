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
  Embedding
, empty
, fromList

  -- * Transforming
, toList

  -- * Querying
, fun

  -- * Modifying
, insert
, update
, resolveX
, resolveY
)
where

  import qualified Data.List       as L
  import qualified Data.Function   as Fun
  import qualified Data.Map.Strict as Map

  import qualified Data.Algorithm.PPattern.Types  as T
  import qualified Data.Algorithm.PPattern.CPoint as CPoint
  import qualified Data.Algorithm.PPattern.Next   as Next

  newtype Embedding =  Embedding { getMap :: Map.Map CPoint.CPoint CPoint.CPoint }
                       deriving (Eq)

  instance Show Embedding where
    show Embedding { getMap=m } = L.intercalate "\n" strList
      where
        asList     = Map.toList m
        sortedList = L.sortBy (compare `Fun.on` (CPoint.xCoord . fst)) asList
        strList    = fmap (\(cp1, cp2) -> show cp1 ++ " -> " ++ show cp2) sortedList

  {-|
  'empty' constructs an empty embedding.
  -}
  empty :: Embedding
  empty  = Embedding Map.empty

  {-|
    Return the image of a colored point.
  -}
  fun :: CPoint.CPoint -> Embedding -> Maybe CPoint.CPoint
  fun cp e = Map.lookup cp (getMap e)

  {-|
    Construct an embedding from a list of key/value pairs.
  -}
  fromList :: [(CPoint.CPoint, CPoint.CPoint)] -> Embedding
  fromList = Embedding . Map.fromList

  {-|
    Transform an embedding into a list of key/value pairs.
  -}
  toList :: Embedding -> [(CPoint.CPoint, CPoint.CPoint)]
  toList = Map.toList . getMap

  {-|
    Insert a key/value in the embedding.
  -}
  insert :: CPoint.CPoint -> CPoint.CPoint -> Embedding -> Embedding
  insert cp cp' Embedding { getMap=m } = Embedding (Map.insert cp cp' m)

  {-|
    Update the embedding function.
  -}
  update :: CPoint.CPoint -> CPoint.CPoint -> Embedding -> Embedding
  update cp cp' Embedding { getMap=m } = Embedding (Map.update (\_ -> Just cp') cp m)

  {-|
  -}
  resolveX :: CPoint.CPoint -> T.T -> Next.Next -> Embedding -> Maybe Embedding
  resolveX cp thrshld n e = fun cp e >>= Next.jmpThresholdXQ thrshld n >>= resolveAux cp n e

  {-|
  -}
  resolveY :: CPoint.CPoint -> T.T -> Next.Next -> Embedding -> Maybe Embedding
  resolveY cp thrshld n e = fun cp e >>= Next.jmpThresholdYQ thrshld n >>= resolveAux cp n e

  {-|
  -}
  resolveAux :: CPoint.CPoint -> Next.Next -> Embedding -> Int -> Maybe Embedding
  resolveAux cp1 n e k = fun cp1 e >>= Next.nextQK k n >>= aux
    where
      aux :: CPoint.CPoint -> Maybe Embedding
      aux cp2 = case Next.nextP cp1 n of
                   Nothing   -> Just e'
                   Just cp1' -> resolveAux cp1' n e' k
        where
          e' = update cp1 cp2 e
