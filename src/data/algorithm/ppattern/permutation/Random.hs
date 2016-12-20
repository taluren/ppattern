{-|
Module      : Data.Algorithm.PPattern.Permutation.Random
Description : Short description
Copyright   : (c) Stéphane Vialette,2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module,containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Permutation.Random
(
  randPermutation
, randPermutation'
, randKIncreasing
, randKIncreasings
)
where

  import qualified Data.Algorithm.PPattern.Permutation             as Permutation
  import qualified Data.Algorithm.PPattern.Permutation.Stringology as Stringology
  import qualified Data.Algorithm.PPattern.Random                  as Random

  {-|
    'randPermutation' takes a permutation 'p' and a generator 'g', and
    returns a random permutation of 'p', together with a new generator.
  -}
  randPermutation :: R.RandomGen g => Permutation.Permutation -> g -> (Permutation.Permutation, g)
  randPermutation = Permutation.fromListUnsafe . Random.randPermutation . Permutation.toList

  {-|
    'randPermutation'' takes an integer 'n' and a generator 'g', and
    returns a random permutation of '[1..n]', together with a new generator.
  -}
  randPermutation' :: R.RandomGen g => Int -> g -> (Permutation.Permutation, g)
  randPermutation' n = randPermutation p
    where
      p = Permutation.increasing n

  -- {-|
  --   'randKIncreasing' takes two integers 'n' and 'k' and a generator 'g'.
  --   It returns a random permutation of length 'n' that is the union of 'k'
  --   increasings sequences, together with a new generator.
  -- -}
  randKIncreasing :: R.RandomGen g => Int -> Int -> g -> (Permutation.Permutation, g)
  randKIncreasing n k g =
    if Stringology.lenLongestDecreasingSub p > k
      then randKIncreasing n k g'
      else (p, g')
    where
      (p, g') = randPermutation' n g

  {-|
    'randKIncreasings' takes three integers 'n', 'k' and 'm' and a generator 'g'.
    It returns 'm' random permutations of length 'n' (each permutation is the
    union of 'k' increasings sequences), together with a new generator.
  -}
  randKIncreasings :: (R.RandomGen g) => Int -> Int -> Int -> g -> ([Permutation.Permutation], g)
  randKIncreasings n k = aux []
    where
      aux acc 0 g = (acc, g)
      aux acc m g = aux (xs:acc) (m-1) g'
        where
          (xs, g') = randKIncreasing n k g
