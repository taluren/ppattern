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
)
where

  -- import qualified Data.List  as L
  import qualified Data.Tuple as T

  removeAt :: (Eq a, Num a) => [b] -> a -> (b, [b])
  removeAt []     _ = error "Cannot removeAt an empty list"
  removeAt (x:xs) 0 = (x, xs)
  removeAt (x:xs) n = (x', x:xs')
    where
      (x', xs') = removeAt xs (n-1)

  removeAt' :: (Eq a, Num a) => [b] -> a -> [b]
  removeAt' xs i = T.snd $ removeAt xs i
