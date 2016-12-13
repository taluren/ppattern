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
  --
, fromSeq,
, fromList
  --
, toList
  --
, reduce
)
where

  import qualified Data.List as L

  import qualified Data.Algorithm.PPattern.Seq as Seq

  data Permutation

  fromSeq :: Seq.Seq t a -> Seq.Seq Permutation a
  fromSeq (Seq.Seq xs) = (Seq.Seq xs)

  fromList :: [a] -> Seq.Seq Permutation a
  fromList = Seq.Seq

  toList :: Seq.Seq Permutation a -> [a]
  toList = Seq.toList
