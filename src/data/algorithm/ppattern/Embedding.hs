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

  import qualified Data.Map.Strict as Map

  import qualified Data.Algorithm.PPattern.CPoint as CPoint
  import qualified Data.Algorithm.PPattern.Next   as Next

  type Embedding =  Map.Map CPoint.CPoint CPoint.CPoint

  {-|
  'empty' constructs an empty embedding.
  -}
  empty :: Embedding
  empty  = Map.empty

  {-|

  -}
  fun :: CPoint.CPoint -> Embedding -> Maybe CPoint.CPoint
  fun = Map.lookup

  {-|

  -}
  fromList :: [(CPoint.CPoint, CPoint.CPoint)] -> Embedding
  fromList = Map.fromList


  {-|

  -}
  toList :: Embedding -> [(CPoint.CPoint, CPoint.CPoint)] 
  toList = Map.toList

  {-|
    Insert a key/value in the embedding.
  -}
  insert :: CPoint.CPoint -> CPoint.CPoint -> Embedding -> Embedding
  insert = Map.insert

  {-|
  -}
  update :: CPoint.CPoint -> CPoint.CPoint -> Embedding -> Embedding
  update cp cp' = Map.update (\_ -> Just cp') cp

  {-|
  -}
  resolveX :: CPoint.CPoint -> Int -> Next.Next -> Embedding -> Maybe Embedding
  resolveX cp thrshld n e = fun cp e >>= Next.jumpThresholdXQ thrshld n >>= resolveAux cp n e

  {-|
  -}
  resolveY :: CPoint.CPoint -> Int -> Next.Next -> Embedding -> Maybe Embedding
  resolveY cp thrshld n e = fun cp e >>= Next.jumpThresholdYQ thrshld n >>= resolveAux cp n e

  {-|
  -}
  resolveAux :: CPoint.CPoint -> Next.Next -> Embedding -> Int -> Maybe Embedding
  resolveAux cp1 n e k = fun cp1 e >>= Next.nextQK k n >>= aux
    where
      aux :: CPoint.CPoint -> Maybe Embedding
      aux cp1' = case Next.nextP cp1 n of
                   Nothing  -> Just m'
                   Just cp2 -> resolveAux cp2 n m' k
        where
          m' = update cp1 cp1' e
