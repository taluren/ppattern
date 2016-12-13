{-|
Module      : Data.Algorithm.PPattern.Generator
Description : Short description
Copyright   : (c) Stéphane Vialette,2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module,containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Generator
(
  randomPermutations
)
where

  import qualified Data.List       as L
  import qualified Data.Tuple      as T
  import qualified Data.Map.Strict as M
  import qualified Data.Foldable   as F
  import qualified System.Random         as Random
  import qualified System.Random.Shuffle as Random.Shuffle

  import qualified Data.Algorithm.PPattern.Seq as Seq

  {-|
    The 'randomPermutations' function returns an infinite list of random
    permutation of length 'n'.
  -}
  randomPermutations :: Int -> [Seq.Seq Seq.Permutation Int]
  randomPermutations n = L.map Seq.permutationFromList $ aux (Random.mkStdGen 100)
    where
      aux g = (Random.Shuffle.shuffle' xs n g):aux (nextGenerator g)

      xs = [1..n]

      nextGenerator :: Random.StdGen -> Random.StdGen
      nextGenerator g = T.snd (Random.random g :: (Int,Random.StdGen))
