{-|
Module      : Data.Algorithm.PPattern.Seq
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Seq
(
  Seq
, Isogram
, Permutation
  --
, fromList
, isogramFromList
, permutationFromList
  --
, toList
  --
, Data.Algorithm.PPattern.Seq.length
)
where

  import qualified Data.List  as L
  import qualified Data.Tuple as T

  -- |Sequnce data type.
  newtype Seq t a = Seq [a]

  -- |Isogram Sequence annotation.
  data Isogram

  -- |Permutation Sequence annotation.
  data Permutation

  instance (Show a) => Show (Seq t a) where
    show (Seq xs) = "Seq Permutation " ++ (show xs)

  instance (Eq a) => Eq (Seq t a) where
    (Seq xs) == (Seq ys) = xs == ys

  fromList :: [a] -> Seq t a
  fromList = Seq

  isogramFromList :: [a] -> Seq Isogram a
  isogramFromList = fromList

  permutationFromList :: (Ord a, Num a, Enum a) => [a] -> Seq Permutation a
  permutationFromList = reduce . fromList

  toList :: Seq t a -> [a]
  toList (Seq xs) = xs

  length :: Seq t a -> Int
  length = L.length . toList

  reduce :: (Ord a, Num a, Enum a) => Seq t a -> Seq Permutation a
  reduce = fromList . getReducedValues . sortByIndex . makeIndex . sortByValue . makeIndex . toList
    where
      makeIndex     = L.zip [1..]
      sortByValue   = L.sortBy (\t1 t2 -> (T.snd t1) `compare` (T.snd t2))
      sortByIndex   = L.sortBy (\t1 t2 -> (T.fst (T.snd t1)) `compare` (T.fst (T.snd t2)))
      getReducedValues = L.map (T.fst)
