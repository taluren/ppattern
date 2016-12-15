{-|
Module      : Data.Algorithm.PPattern.Tools
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Tools
(
  removeAt
, removeAt'
, sortPairFst
, sortPairSnd
)
where

  import qualified Data.List  as L
  import qualified Data.Tuple as T

  removeAt :: (Eq a, Num a) => [b] -> a -> (b, [b])
  removeAt (x:xs) 0 = (x, xs)
  removeAt (x:xs) n = (x', x:xs')
	 where
     (x', xs') = removeAt xs (n-1)

  removeAt' :: (Eq a, Num a) => [b] -> a -> [b]
  removeAt' xs i = T.snd $ removeAt xs i

  {-|
    The 'square' function squares an integer.
    It takes one argument, of type 'Int'.
  -}
  sortPairFst :: Ord a => [(a, b)] -> [(a, b)]
  sortPairFst = L.sortBy comp
    where
      comp t1 t2 = (T.fst t1) `compare` (T.fst t2)

  {-|
    The 'square' function squares an integer.
    It takes one argument, of type 'Int'.
  -}
  sortPairSnd :: Ord b => [(a, b)] -> [(a, b)]
  sortPairSnd = L.sortBy comp
    where
      comp t1 t2 = (T.snd t1) `compare` (T.snd t2)
