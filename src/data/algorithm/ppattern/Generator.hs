{-|
Module      : Data.Algorithm.PPattern.Generator
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Generator
(
  random123Avoiding
, randomBinaryList
)
where

  import qualified Data.List        as L
  import qualified System.Random    as Random
  import qualified System.IO.Unsafe as Unsafe

  import Data.Algorithm.PPattern.Types

  random123Avoiding :: Length -> Permutation
  random123Avoiding n = [1..n]

  randomBinaryList :: Length -> [Int]
  randomBinaryList n = L.take n $ Random.randomRs (0,1) $ g
    where
      g = Unsafe.unsafePerformIO Random.newStdGen
