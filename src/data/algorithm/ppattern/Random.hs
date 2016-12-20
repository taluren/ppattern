{-|
Module      : Data.Algorithm.PPattern.Random
Description : Short description
Copyright   : (c) Stéphane Vialette,2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module,containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Random
(
  randSelect
, randPermutation
, randPermutation'
, randIntPartitionByL
, randKIncreasing
, randKIncreasingL
, randShuffle
)
where

  import qualified Data.List       as L
  import qualified Data.Foldable   as Fold
  import qualified System.Random   as R

  import qualified Data.Algorithm.PPattern.Tools                   as Tools
  import qualified Data.Algorithm.PPattern.Combinatorics           as Combinatorics
  import qualified Data.Algorithm.PPattern.IntPartition            as IntPartition
  import qualified Data.Algorithm.PPattern.Permutation             as Permutation
  import qualified Data.Algorithm.PPattern.Permutation.Stringology as Stringology

  {-|
    'randChoose' takes a list 'xs', an integer 'k' and a generator 'g', and
    returns a random sublist of 'xs' of length 'k'), together with a new generator.
  -}
  randChoose :: R.RandomGen g => [a] -> Int -> g -> ([a], g)
  randChoose xs k g = (xss L.!! (i-1), g')
    where
      xss = xs `Combinatorics.choose` k
      (i, g') = R.randomR (1, L.length xss) g

  {-|
    'randSelect' takes a list 'xs', an integer 'k' and a generator 'g', and
    returns a random list 'xs' of length 'k' of random selected elements from
    'xs', together with a new generator.
  -}
  randSelect :: R.RandomGen g => [a] -> Int -> g -> ([a], g)
  randSelect xs = aux xs []
     where
        aux xs acc k g
          | k == 0    = (acc, g)
          | otherwise = aux (Tools.removeAt' xs i) (x:acc) (k-1) g'
          where
            n       = L.length xs
            (i, g') = R.randomR (0, n-1) g
            x       = xs L.!! i

  {-|
    'randPermutation' takes a list 'xs' and a generator 'g', and
    returns a random permutation of 'xs', together with a new generator.
  -}
  randPermutation :: R.RandomGen g => Permutation.Permutation -> g -> (Permutation.Permutation, g)
  randPermutation (Permutation xs) = Permutation.fromListUnsafe $ randSelect xs (L.length xs)

  {-|
    'randPermutation'' takes an integer 'n' and a generator 'g', and
    returns a random permutation of '[1..n]', together with a new generator.
  -}
  randPermutation' :: R.RandomGen g => Int -> g -> (Permutation.Permutation, g)
  randPermutation' n = randPermutation [1..n]

  {-|
    'randIntPartitionByL' takes two integers 'n' and 'k', and a generator 'g', and
    returns a random 'k'-partition of '[1..n]', together with a new generator.
  -}
  randIntPartitionByL :: (R.RandomGen g) => Int -> Int -> g -> ([Int], g)
  randIntPartitionByL n k g = (L.head ips, g')
    where
      (ips, g') = randSelect (IntPartition.intPartitionsByL n k) 1 g

  {-|
    'randShuffle' takes a list of lists 'xss' and a generator 'g', and
    returns a random shuffle of xss (i.e. each list of 'xss' is a sublist of the
    result), together with a new generator.
  -}
  randShuffle :: R.RandomGen g => [[a]] -> g -> ([a], g)
  randShuffle xss = aux xss []
    where
      aux []  acc g = (L.reverse acc, g)
      aux xss acc g = aux xss'' (x:acc) g'
        where
          (xss', g') = randPermutation xss g
          (x, xss'') = aux' xss'
            where
              aux' ([x]:xss)     = (x, xss)
              aux' ((x:xs):xss') = (x, xs:xss')

  -- {-|
  --   'randKIncreasing' takes two integers 'n' and 'k' and a generator 'g', and
  --   returns a random permutation of length 'n' that is the union of 'k'
  --   increasings sequences, together with a new generator.
  -- -}
  -- randKIncreasing:: R.RandomGen g => Int -> Int -> g -> ([Int], g)
  -- randKIncreasing n k g = (xs, g'')
  --   where
  --     (ip, g')   = randIntPartitionByL n k g
  --     (xss, g'') = randKIncreasingL ip g'
  --     (xs, g''') = randShuffle xss g''

  randKIncreasing:: R.RandomGen g => Int -> Int -> g -> ([Int], g)
  randKIncreasing n k g =
    if Stringology.lenLongestDecreasingSub xs > k
      then randKIncreasing n k g'
      else (xs, g')
    where
      (xs, g') = randPermutation' n g

  {-|
    'randKIncreasings' takes three integers 'n', 'k' and 'm' and a generator 'g',
    and returns 'm' random permutations of length 'n' that are the union of 'k'
    increasings sequences, together with a new generator.
  -}
  randKIncreasings :: (R.RandomGen g) => Int -> Int -> Int -> g -> ([[Int]], g)
  randKIncreasings n k = aux []
    where
      aux acc 0 g = (acc, g)
      aux acc m g = aux (xs:acc) (m-1) g'
        where
          (xs, g') = randKIncreasing n k g

  {-|
    'randKIncreasingL'
  -}
  randKIncreasingL :: R.RandomGen g => [Int] -> g -> ([[Int]], g)
  randKIncreasingL ls = aux ls [1..Fold.sum ls] []
    where
      aux []     [] acc g = (acc, g)
      aux (l:ls) xs acc g = aux ls (xs L.\\ xs') (xs':acc) g'
        where
          (xs', g') = randChoose xs l g
