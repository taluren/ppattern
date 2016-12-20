{-|
Module      : Data.Algorithm.PPattern.IntPartition.Random
Description : Short description
Copyright   : (c) Stéphane Vialette,2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module,containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.IntPartition.Random
(
  randPermutation
, randPermutation'
, randKIncreasing
, randKIncreasings
)
where

  import qualified Data.Algorithm.PPattern.IntPartition as IntPartition
  import qualified Data.Algorithm.PPattern.Random       as Random

  {-|
    'randIntPartitionByL' takes two integers 'n' and 'k', and a generator 'g'.
    It returns a random 'k'-partition of '[1..n]', together with a new generator.
  -}
  randIntPartitionByL :: (R.RandomGen g) => Int -> Int -> g -> (IntPartition, g)
  randIntPartitionByL n k g = (IntPartition.mkIntPartition ip, g')
    where
      (ip, g') = Random.randIntPartitionByL n k g
