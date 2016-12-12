{-|
Module      : Data.Algorithm.PPattern.Permutation
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Permutation
(
  Permutation
)
where

  newtype Permutation = Permutation { toList :: [a] }
    deriving (Eq, Show, Ord)

  fromList :: [a] -> Permutation a
  fromList = Permutation . reduce

  toIsogram :: Permutation a -> Isogram a
  toIsogram = Isogram.fromList . toList

  reduce :: [a] -> Permutation a
  reduce = reducedValues . sortByIndex . makeIndex . sortByValue . makeIndex
    where
      makeIndex     = L.zip [1..]
      sortByValue   = L.sortBy (\t1 t2 -> (T.snd t1) `compare` (T.snd t2))
      sortByIndex   = L.sortBy (\t1 t2 -> (T.fst (T.snd t1)) `compare` (T.fst (T.snd t2)))
      reducedValues = L.map (T.fst)
