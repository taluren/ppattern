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
  Permutation(..)
  ---
, fromList
, fromListUnsafe
, empty
  ---
, toList
  ---
, reduce
  ---
, Data.Algorithm.PPattern.Permutation.Core.length
  ---
, partitionsIncreasings
, greedyIncreasing1
, greedyPartitionIncreasings1
, greedyIncreasing2
, greedyPartitionIncreasings2
  ---
, longestIncreasingSub
, lenLongestIncreasingSub
, longestDecreasingSub
, lenLongestDecreasingSub
  ---
, randPermutation
, randPermutation'
, randKIncreasing
, randKIncreasings
)
where

  import qualified Data.List     as L
  import qualified Data.Foldable as Fold
  import qualified Data.Tuple    as T
  import qualified Data.Set      as Set
  import qualified Data.Monoid   as Monoid
  import qualified Data.Function as Fun
  import qualified System.Random as R
  import Control.Applicative

  type TPermutation = Int

  newtype Permutation = Permutation [TPermutation] deriving (Show, Eq, Ord)

  {-|
    'fromList xs' construct a reduced permutation from list xs.
  -}
  fromList ::  [TPermutation] -> Permutation
  fromList = reduce . fromListUnsafe

  {-|
    'fromList xs' constructs a permutation from the list xs. No check is done!
  -}
  fromListUnsafe :: [TPermutation] -> Permutation
  fromListUnsafe = Permutation

  {-|
    'empty' returns the empty permutation (i.e., the permutation with no element).
  -}
  empty :: Permutation
  empty = fromListUnsafe []

  {-|
    'toList p' returns the permutation 'p' as a list.
  -}
  toList :: Permutation -> [TPermutation]
  toList (Permutation xs) = xs

  {-|
    'preIdx p' returns the pre-indexed form of the permutation 'p'.

    λ: preIdx $ fromListUnsafe []
    []
    λ: preIdx $ fromListUnsafe [4,2,1,3]
    [(1,4),(2,2),(3,1),(4,3)]
  -}
  preIdx :: Permutation -> [(Integer, TPermutation)]
  preIdx  = L.zip [1..] . toList

  {-|
    'postIdx p' returns the post-indexed form of the permutation 'p'.

    λ: postIdx $ fromListUnsafe []
    []
    λ: postIdx $ fromListUnsafe [4,2,1,3]
    [(4,1),(2,2),(1,3),(3,4)]
  -}
  postIdx :: Permutation -> [(TPermutation, Integer)]
  postIdx = flip L.zip [1..] . toList

  {-|
    'reduce p' returns the reduced permutation of permutation 'p'.

    λ: reduce []
    []
    λ: reduce [1..5]
    [1,2,3,4,5]
    λ: reduce [5,9,2,7,3]
    [3,5,1,4,2]
  -}
  reduce :: Permutation -> Permutation
  reduce = fromListUnsafe . get . sortByIdx . L.zip [1..] . sortByVal . L.zip [1..] . toList
    where
      sortByVal = L.sortBy (compare `Fun.on` T.snd)
      sortByIdx = L.sortBy (compare `Fun.on` (T.fst . T.snd))
      get       = fmap T.fst


  {-|
    'length p' returns the length of the permutation 'p'.
  -}
  length :: Permutation -> Int
  length = L.length . toList

  {-|
    'increasingsByL xs k' return the list of all increasing subsequences
    of length 'k' of the list 'xs'.
  -}
  increasingsByL' :: [Int] ->  Int -> [[Int]]
  increasingsByL' [] _ = []
  increasingsByL' xs k = aux xs k (L.head xs)
    where
      aux _      0 _  = [[]]
      aux []     _ _  = []
      aux (x:xs) k' x'
        | k == k' || x > x' = fmap (x:) xss `Monoid.mappend` aux xs k' x'
        | otherwise         = aux xs k' x'
        where
          xss = aux xs (k'-1) x

  {-|
    'increasingsByL p k' return the list of all increasing subsequences
    of length 'k' of the permutation 'p'.
  -}
  increasingsByL :: Permutation.Permutation ->  Int -> [Permutation.Permutation]
  increasingsByL (Permutation.Permutation xs) k = fmap Permutation.fromListUnsafe (increasingsByL' xs k)

  {-|
    'partitionsIncreasingsByL xs ks' returns all partitions of 'xs' into |ks|
    increasing subsequences of length ks = [k1, k2, ..., kp].
  -}
  partitionsIncreasingsByL :: Permutation.Permutation -> IntPartition.IntPartition -> [[Permutation.Permutation]]
  partitionsIncreasingsByL p intPartition = fmap Permutation.fromListUnsafe <$> aux xs ls
    where
      xs = Permutation.toList p

      ls = IntPartition.toList intPartition

      aux [] []     = [[]]
      aux [] _      = []
      aux _  []     = []
      aux xs (l:ls) = [is:iss |
                       is  <- increasingsByL' xs l,
                       iss <- aux (xs L.\\ is) ls]

  {-|
    'isClassLeader xss' returns 'True' if and only if xss is composed of
    ascending sorted list, each list being sorted ascending.
  -}
  isClassLeader :: [Permutation.Permutation] -> Bool
  isClassLeader ps = xss == xss'
    where
      xss = [Permutation.toList p | p <- ps]
      xss' = L.sort . fmap L.sort $ fmap Permutation.toList ps

  {-|
    'partitionsIncreasings p n' return all partitions of permutation 'p' into 'k'
    increasing subsequences.
  -}
  partitionsIncreasings :: Permutation.Permutation -> Int -> [[Permutation.Permutation]]
  partitionsIncreasings p@(Permutation.Permutation xs) k
    | Stringology.lenLongestDecreasingSub p > k = []
    | otherwise                                 = aux xs k
    where
      aux xs k = [pPartition |
                  intPartition <- IntPartition.intPartitionsByL n k,
                  pPartition   <- partitionsIncreasingsByL p intPartition,
                  isClassLeader pPartition]
      n = Permutation.length p

  {-|
    'greedyPartitionIncreasings xs f' return a partition of xs into increasing
    subsequences by repeatedly calling function 'f' on the remaining subsequence.
  -}
  greedyPartitionIncreasings :: (Permutation.Permutation -> Permutation.Permutation) -> Permutation.Permutation -> [Permutation.Permutation]
  greedyPartitionIncreasings f = aux []
    where
      aux acc (Permutation.Permutation []) = acc
      aux acc p                            = aux (q1:acc) q2
        where
          q1  = f p

          q2 = p Permutation.\\ q1

  {-|
    'greedyPartitionIncreasings1' takes a list 'xs'. It greedily computes a partition
    of 'xs' into increasing subsequences.
  -}
  greedyPartitionIncreasings1 :: Permutation.Permutation -> [Permutation.Permutation]
  greedyPartitionIncreasings1 = greedyPartitionIncreasings greedyIncreasing1

  {-|
    'greedyIncreasing1' takes a list 'xs'. It greedily computes an increasing
    subsequence of 'xs'.
  -}
  greedyIncreasing1 :: Permutation.Permutation -> Permutation.Permutation
  greedyIncreasing1 (Permutation.Permutation [])     = Permutation.empty
  greedyIncreasing1 (Permutation.Permutation (x:xs)) = Permutation.fromListUnsafe $ x:aux x xs
    where
      aux _ []      = []
      aux x (x':xs)
        | x' > x    = x':aux x' xs
        | otherwise =    aux x  xs

  {-|
    'greedyPartitionIncreasings1' takes a list 'xs'. It greedily computes a partition
    of 'xs' into increasing subsequences.
  -}
  greedyPartitionIncreasings2 :: Permutation.Permutation -> [Permutation.Permutation]
  greedyPartitionIncreasings2 = greedyPartitionIncreasings greedyIncreasing2


  {-|
    'greedyIncreasing1' takes a list 'xs'. It greedily computes an increasing
    subsequence of 'xs'.
  -}
  greedyIncreasing2 :: Permutation.Permutation -> Permutation.Permutation
  greedyIncreasing2 = Stringology.longestIncreasingSub

  {-|
    'longestIncreasingSub xs' returns a longest increasing subsequences in 'xs'.
  -}
  longestIncreasingSub :: Permutation.Permutation -> Permutation.Permutation
  longestIncreasingSub = post . Patience.longestIncreasing . pre
    where
      pre  = flip L.zip [1..] . Permutation.toList
      post =  Permutation.fromListUnsafe . L.map T.fst . L.reverse

  {-|
    'lenLongestIncreasingSub xs' returns the length of the longest increasing
    subsequences in 'xs'.
  -}
  lenLongestIncreasingSub:: Permutation.Permutation -> Int
  lenLongestIncreasingSub = Permutation.length . longestIncreasingSub

  {-|
    'longestDecreasingSub xs' returns a longest decreasing subsequences in 'xs'.
  -}
  longestDecreasingSub :: Permutation.Permutation -> Permutation.Permutation
  longestDecreasingSub = post . Patience.longestIncreasing . pre
    where
      pre  = flip L.zip [1..] . L.reverse . Permutation.toList
      post = Permutation.fromListUnsafe . L.reverse . L.map T.fst . L.reverse

  {-|
    'lenLongestDecreasingSub xs' returns the length of the longest decreasing
    subsequences in 'xs'.
  -}
  lenLongestDecreasingSub :: Permutation.Permutation -> Int
  lenLongestDecreasingSub = Permutation.length . longestDecreasingSub

  {-|
    'randPermutation' takes a permutation 'p' and a generator 'g', and
    returns a random permutation of 'p', together with a new generator.
  -}
  randPermutation :: R.RandomGen g => Permutation.Permutation -> g -> (Permutation.Permutation, g)
  randPermutation p g = (Permutation.fromListUnsafe xs, g')
    where
      (xs, g') = flip Random.randPermutation g $ Permutation.toList p

  {-|
    'randPermutation'' takes an integer 'n' and a generator 'g', and
    returns a random permutation of '[1..n]', together with a new generator.
  -}
  randPermutation' :: R.RandomGen g => Int -> g -> (Permutation.Permutation, g)
  randPermutation' n g = (p', g')
    where
      p        = Permutation.increasing n
      (p', g') = randPermutation p g

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
