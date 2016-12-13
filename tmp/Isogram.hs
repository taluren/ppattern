{-|
Module      : Data.Algorithm.PPattern.Isogram
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Isogram
(
  Isogram
, fromSeq
, fromList
  --
, toList
  --
, length
)
where

  import qualified Data.Algorithm.PPPattern.Seq as Seq

  data Isogram

  fromSeq :: Seq.Seq t a -> Seq.Seq Isogram a
  fromSeq (Seq.Seq xs) = (Seq.Seq xs)

  toIsogram :: [a] -> Seq.Seq Isogram a
  toIsogram = Seq.Seq

  toList :: Seq.Seq Isogram a -> [a]
  toList (Seq.Seq xs) = Seq.toList

  length ::
