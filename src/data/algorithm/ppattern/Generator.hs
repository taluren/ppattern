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
  removeAt
, randSelect
, randPermutation
)
where

  import qualified Data.List       as L
  import qualified Data.Tuple      as T
  import qualified Data.Map.Strict as M
  import qualified Data.Foldable   as F
  import qualified System.Random   as Random
  import qualified Control.Monad   as Monad
  import qualified Control.Monad.Random as Monad.Random


  import qualified Data.Algorithm.PPattern.Seq as Seq
  import qualified Data.Algorithm.PPattern.IntPartition as IntPartition

  removeAt :: (Eq a, Num a) => a -> [b] -> (b, [b])
  removeAt 1 (x:xs) = (x, xs)
  removeAt n (x:xs) = (l, x:r)
	 where
     (l, r) = removeAt (n - 1) xs

  randSelect :: RandomGen g => [a] -> Int -> g -> ([a], g)
  randSelect _  0 g = ([], g)
  randSelect [] _ g = ([], g)
  randSelect l  k g
    | k == (L.length l) = (l, g)
    | otherwise         = randSelect (removeAt l (k+1)) k' g'
                            where
                              (k', g') = Random.randomR (0, (L.length l) - 1) g

  randPermutation :: RandomGen g => [a] -> g -> ([a], g)
  randPermutation xs = randSelect xs (L.length xs)

  {-|
    The 'randomPermutations' function returns an infinite list of random
    permutation of length 'n'.
  -}
  randIntPartitionByLength :: Int -> Int -> Random.StdGen -> (IntPartition.IntPartition Int, Random.StdGen)
  randIntPartitionByLength n k g = aux $ randSelect ips g
    where
      ips = IntPartition.partitionsByLength n k

      aux ([],       g) = ([], g)
      aux ((ip:ips), g) = (ip, g)

  {-|
    The 'randomPermutations' function returns an infinite list of random
    permutation of length 'n'.
  -}
  -- randomPermutation :: Int -> Random.StdGen -> (Seq.Seq Seq.Permutation Int, Random.StdGen)
  -- randomPermutation n g = (Seq.permutationFromList xs,g')
  --   where
  --     (xs,g') = shuffle [1..n] g
  --
  -- randomPermutations :: Int -> Int -> Random.StdGen -> ([Seq.Seq Seq.Permutation Int], Random.StdGen)
  -- randomPermutations n m g = (L.map T.fst ps, T.snd (L.last ps))
  --   where
  --     ps = aux n m g
  --       where
  --         aux _ 0 g = [(Seq.permutationFromList [], g)]
  --         aux n m g = (p,g'):aux n (m-1) g'
  --           where
  --             (p,g') = randomPermutation n g
  --
  -- f :: [Int] -> [Int] -> Random.StdGen -> ([Int], Random.StdGen)
  -- f [] [] = []
  -- f (l:ls) xs g = ys:yss
  --   where
  --     ys =
  {-|
    The 'randomPermutations' function returns an infinite list of random
    permutation of length 'n'.
  -}
  -- randomKIncreasingPermutation :: Int -> Int -> Random.StdGen -> (Seq.Seq Seq.Permutation Int, Random.StdGen)
  -- randomKIncreasingPermutation n l g = (p, g'')
  --   where
  --     (ls, g') = randomIntPartitionByLength n l g
  --     p = f [1..n] (Seq.toList q)
  --
  -- f :: Seq.Seq Seq.Permutation Int -> [Int] -> [Seq.Seq Seq.Isogram Int]
  -- f s = F.map Seq.isogramFromList . aux (Seq.toList s)
  -- where
  --   aux _ [] = []
  --   aux xs (l:ls) = ys:aux ys' ls
  --   where
  --     (ys, ys') = L.splitAt l xs
  --
  -- randomKIncreasingPermutations :: Int -> Int -> Int -> Random.StdGen -> [(Seq.Seq Seq.Permutation Int, Random.StdGen)]
  -- randomKIncreasingPermutations _ 0 g = (p, nextStdGen g)
  -- randomKIncreasingPermutations n k g = p:randomKIncreasingPermutations n (k-1) g'
  --   where
  --     (intPartition,g') = randomIntPartitionByLength n k g
  --     (p,g'')           = randomPermutation n g'
