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
  Permutation(..)
  ---
, fromList
, fromListUnsafe
  ---
, increasing
, decreasing
, empty
  --
, toList
  --
, Data.Algorithm.PPattern.Permutation.length
  --
, preIdx
, postIdx
  ---
, reduce
)
where

  import qualified Data.List     as L
  import qualified Data.Tuple    as T
  import qualified Data.Function as Fun

  type TPermutation = Int

  newtype Permutation = Permutation [TPermutation] deriving (Show, Eq, Ord)

  {-|
    'fromList xs' construct a reduced permutation from list xs.
  -}
  fromList ::  [TPermutation] -> Permutation
  fromList = reduce . fromListUnsafe

  {-|
    'fromList xs' constructs a permutation from the list xs. No check is done!
  -}
  fromListUnsafe :: [TPermutation] -> Permutation
  fromListUnsafe = Permutation

  {-|
    'increasing n' return the increasing permutation 1 2 ... n. The function
    returns the empty permutation if 'n' is non-positive.
  -}
  increasing :: Int -> Permutation
  increasing n = fromListUnsafe [1..n]

  {-|
    'decrasing n' return the decreasing permutation n n-1 ... 1. The function
    returns the empty permutation if 'n' is non-positive.
  -}
  decreasing :: Int -> Permutation
  decreasing n = fromListUnsafe [n,n-1..1]

  {-|
    'empty' returns the empty permutation (i.e., the permutation with no element).
  -}
  empty :: Permutation
  empty = fromListUnsafe []

  {-|
    'toList p' returns the permutation 'p' as a list.
  -}
  toList :: Permutation -> [TPermutation]
  toList (Permutation xs) = xs

  {-|
  -}
  length :: Permutation -> Int
  length = L.length . toList

  {-|
    'preIdx p' returns the pre-indexed form of the permutation 'p'.

    λ: preIdx $ fromListUnsafe []
    []
    λ: preIdx $ fromListUnsafe [4,2,1,3]
    [(1,4),(2,2),(3,1),(4,3)]
  -}
  preIdx :: Permutation -> [(Integer, TPermutation)]
  preIdx  = L.zip [1..] . toList

  {-|
    'postIdx p' returns the post-indexed form of the permutation 'p'.

    λ: postIdx $ fromListUnsafe []
    []
    λ: postIdx $ fromListUnsafe [4,2,1,3]
    [(4,1),(2,2),(1,3),(3,4)]
  -}
  postIdx :: Permutation -> [(TPermutation, Integer)]
  postIdx = flip L.zip [1..] . toList

  {-|
    'reduce p' returns the reduced permutation of permutation 'p'.

    λ: reduce []
    []
    λ: reduce [1..5]
    [1,2,3,4,5]
    λ: reduce [5,9,2,7,3]
    [3,5,1,4,2]
  -}
  reduce :: Permutation -> Permutation
  reduce = fromListUnsafe . get . sortByIdx . L.zip [1..] . sortByVal . L.zip [1..] . toList
    where
      sortByVal = L.sortBy (compare `Fun.on` T.snd)
      sortByIdx = L.sortBy (compare `Fun.on` (T.fst . T.snd))
      get       = fmap T.fst
