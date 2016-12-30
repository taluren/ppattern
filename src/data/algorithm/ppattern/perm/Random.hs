{-|
Module      : Data.Algorithm.PPattern.Perm.Random
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Perm.Random
(
  randPerm
, randPerm'
, randKIncreasing
, randKIncreasings
)
where

  import qualified System.Random as Random

  import Data.Algorithm.PPattern.Perm.Internal

  {-|
    'randPerm' takes a permutation 'p' and a generator 'g', and
    returns a random permutation of 'p', together with a new generatoRandom.
  -}
  randPerm :: Random.RandomGen g => Perm -> g -> (Perm, g)
  randPerm p g = (fromListUnsafe xs, g')
    where
      (xs, g') = flip Random.randPerm g $ toList p

  {-|
    'randPerm'' takes an integer 'n' and a generator 'g', and
    returns a random permutation of '[1..n]', together with a new generatoRandom.
  -}
  randPerm' :: Random.RandomGen g => Int -> g -> (Perm, g)
  randPerm' n g = (p', g')
    where
      p        = mkIncreasing n
      (p', g') = randPerm p g

  -- {-|
  --   'randKIncreasing' takes two integers 'n' and 'k' and a generator 'g'.
  --   It returns a random permutation of length 'n' that is the union of 'k'
  --   increasings sequences, together with a new generatoRandom.
  -- -}
  randKIncreasing :: Random.RandomGen g => Int -> Int -> g -> (Perm, g)
  randKIncreasing n k g =
    if lenLongestDecreasingSub p > k
      then randKIncreasing n k g'
      else (p, g')
    where
      (p, g') = randPerm' n g

  {-|
    'randKIncreasings' takes three integers 'n', 'k' and 'm' and a generator 'g'.
    It returns 'm' random permutations of length 'n' (each Perm is the
    union of 'k' increasings sequences), together with a new generatoRandom.
  -}
  randKIncreasings :: (Random.RandomGen g) => Int -> Int -> Int -> g -> ([Perm], g)
  randKIncreasings n k = aux []
    where
      aux acc 0 g = (acc, g)
      aux acc m g = aux (xs:acc) (m-1) g'
        where
          (xs, g') = randKIncreasing n k g
