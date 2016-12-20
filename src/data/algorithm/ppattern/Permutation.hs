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
, increasings
, decreasing
, empty
  --
, toList
  --
, length
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

  newtype Permutation = Permutation [Int]

  {-|
    'fromList xs' construct a reduced permutation from list xs.
  -}
  fromList ::  [Int] -> Permutation
  fromList = reduce . Permutation

  {-|
    'fromList xs' construct a reduced permutation from list xs.
  -}
  fromListUnsafe :: [Int] -> Permutation
  fromListUnsafe = Permutation

  {-|
  -}
  increasing :: Int -> Permutation
  increasing n = fromListUnsafe [1..n]

  {-|
  -}
  decreasing :: Int -> Permutation
  decreasing n = fromListUnsafe [n,n-1..1]

  {-|
  -}
  empty :: Permutation
  empty = fromListUnsafe []

  {-|
    'toList p' returns the permutation 'p' as a list.
  -}
  toList :: Permutation -> [Int]
  toList (Permutation xs) = xs

  {-|
  -}
  length :: Permutation -> [Int]
  length = L.length . toList

  {-|
    'preIdx p' returns the pre-indexed form of the permutation 'p'.
  -}
  preIdx :: (Enum a, Num a) => Permutation -> Permutation (a, Int)
  preIdx (Permutation xs) = Permutation xs'
    where
        xs' = L.zip [1..] xs

  {-|
    'postIdx p' returns the post-indexed form of the permutation 'p'.
  -}
  postIdx :: (Enum a, Num a) => Permutation -> Permutation (Int, b)
  postIdx (Permutation xs) = Permutation xs'
    where
      xs' = flip L.zip [1..]

  {-|
    'reduce xs' returns the reduced form of 'xs'.

    λ: reduce []
    []
    λ: reduce [1..5]
    [1,2,3,4,5]
    λ: reduce [5,9,2,7,3]
    [3,5,1,4,2]
  -}
  reduce :: Permutation -> Permutation
  reduce = Permutation . get . sortByIdx . preIdx . sortByVal . preIdx . toList
    where
      sortByValue = L.sortBy (compare `Fun.on` T.snd)
      sortByIndex = L.sortBy (compare `Fun.on` (T.fst . T.snd))
      get         = L.map T.fst
