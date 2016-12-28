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

  -- * Querying
, fun

  -- * Modifying
, insert
, update
, resolveX
)
where

  import qualified Data.Map.Strict as Map

  import qualified Data.Algorithm.PPattern.Perm   as Perm
  import qualified Data.Algorithm.PPattern.CPoint as CPoint

  type Embedding =  Map.Map CPoint.CPoint CPoint.CPoint

  {-|
  'empty' constructs an empty embedding.
  -}
  empty :: Embedding
  empty  = Map.empty

  {-|

  -}
  fun :: CPoint.CPoint -> Embedding -> Maybe CPoint.CPoint
  fun  = Map.lookup

  {-|
    Insert a key/value in the embedding.
  -}
  insert :: CPoint.CPoint -> CPoint.CPoint -> Embedding -> Maybe Embedding
  insert = Map.insert
  
  {-|
  -}
  update :: CPoint.CPoint -> CPoint.CPoint -> Embedding -> Embedding
  update cp cp' = Map.update (\_ -> Just cp') cp

  {-|
  -}
  resolveX :: CPoint.CPoint -> Perm.T -> Next.Next -> Next.Next -> Embedding -> Maybe Embedding
  resolveX cp thrshld nextP nextQ m = fun cp m >>= findJump >>= update
    where
      findJump :: Int -> Next -> CPoint.CPoint -> Maybe Int
      findJump = Next.jumpForThresholdX thrshld nextQ

      update :: CPoint.CPoint -> Next.Next -> Next.Next -> Int -> Maybe Embedding
      update = resolveXAux cp nextP nextQ m

  {-|
  -}
  resolveAux :: CPoint.CPoint -> Next -> Next -> Embedding -> Int -> Maybe Embedding
  resolveAux cp1 nextP nextQ m k = fun cp1 m >>= Flippers.flip3 Next.nextK k nextQ >>= aux
    where
      aux :: CPoint.CPoint -> Maybe Embedding
      aux cp1' = case Next.next cp1 nextP of
                   Nothing  -> Just m'
                   Just cp2 -> resolveAux cp2 nextP nextQ m' k
        where
          m' = update cp1 cp1' m
