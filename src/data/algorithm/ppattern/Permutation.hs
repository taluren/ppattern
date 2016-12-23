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
  -- * The @T@ type
  T

  -- * The @Permutation@ type
, Permutation(..)
, fromList
, fromListUnsafe
, mkEmpty
, reduce

  -- * Monotonic permutations
, mkIncreasing
, mkDecreasing

  -- * Conversions
, toList

  -- * Querying
, size

  ---
, diff

  -- * Enumerating subsequences
, increasingsByL

  -- * Partitioning
, partitionsIncreasings
, greedyIncreasing1
, greedyPartitionIncreasings1
, greedyIncreasing2
, greedyPartitionIncreasings2

  -- * LCS
, longestIncreasingSub
, lenLongestIncreasingSub
, longestDecreasingSub
, lenLongestDecreasingSub

  -- * Random
, randPermutation
, randPermutation'
, randKIncreasing
, randKIncreasings
)
where

  import qualified Data.List               as L
  import qualified Data.Tuple              as T
  import qualified Data.Monoid             as Monoid
  import qualified Data.Function           as Fun
  import qualified System.Random           as R
  import qualified Data.Algorithm.Patience as Patience

  import qualified Data.Algorithm.PPattern.Random       as Random
  import qualified Data.Algorithm.PPattern.IntPartition as IntPartition

  -- | Permutation of integers.
  type T = Int

  newtype Permutation = Permutation [T] deriving (Eq, Ord, Show, Read)

  {-|
    'fromList xs' construct a reduced permutation from list xs.
  -}
  fromList ::  [T] -> Permutation
  fromList = reduce . fromListUnsafe

  {-|
    'fromList xs' constructs a permutation from the list xs. No check is done!
  -}
  fromListUnsafe :: [T] -> Permutation
  fromListUnsafe = Permutation

  {-|
    'mkEmpty' returns the empty permutation (i.e., the permutation with no element).
  -}
  mkEmpty :: Permutation
  mkEmpty = fromListUnsafe []

  {-|
  -}
  mkIncreasing :: Int -> Permutation
  mkIncreasing n = fromListUnsafe [1..n]

  {-|
  -}
  mkDecreasing :: Int -> Permutation
  mkDecreasing n = fromListUnsafe [n,n-1..1]

  {-|
    'toList p' returns the permutation 'p' as a list.
  -}
  toList :: Permutation -> [T]
  toList (Permutation xs) = xs

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
  reduce = fromListUnsafe . get . sortByIdx . L.zip is . sortByVal . L.zip is . toList
    where
      sortByVal = L.sortBy (compare `Fun.on` T.snd)
      sortByIdx = L.sortBy (compare `Fun.on` (T.fst . T.snd))
      get       = fmap T.fst
      is        = [1..] :: [T]

  {-|
    'size p' returns the length of the permutation 'p'.
  -}
  size :: Permutation -> Int
  size = L.length . toList

  {-|
    'diff p q' returns the difference permutation of 'p' and 'q'.
  -}
  diff :: Permutation -> Permutation -> Permutation
  diff p q = fromListUnsafe zs
    where
      zs = toList p L.\\ toList q

  {-|
    'increasingsByL xs k' return the list of all increasing subsequences
    of length 'k' of the list 'xs'.
  -}
  increasingsByL' :: [Int] ->  Int -> [[Int]]
  increasingsByL' [] _ = []
  increasingsByL' xs k = increasingsByLAux' xs k k (L.head xs)

  increasingsByLAux' ::  [Int] ->  Int -> Int -> Int -> [[Int]]
  increasingsByLAux' _      _ 0 _  = [[]]
  increasingsByLAux' []     _ _ _  = []
  increasingsByLAux' (x:xs) k k' x'
    | k == k' || x > x' = fmap (x:) xss `Monoid.mappend` increasingsByLAux' xs k k' x'
    | otherwise         = increasingsByLAux' xs k k' x'
    where
      xss = increasingsByLAux' xs k (k'-1) x

  {-|
    'increasingsByL p k' return the list of all increasing subsequences
    of length 'k' of the permutation 'p'.
  -}
  increasingsByL :: Permutation ->  Int -> [Permutation]
  increasingsByL (Permutation xs) k = fmap fromListUnsafe (increasingsByL' xs k)

  {-|
    'partitionsIncreasingsByL xs ks' returns all partitions of 'xs' into |ks|
    increasing subsequences of length ks = [k1, k2, ..., kp].
  -}
  partitionsIncreasingsByL :: Permutation -> IntPartition.IntPartition -> [[Permutation]]
  partitionsIncreasingsByL p intPartition = fmap fromListUnsafe <$> partitionsIncreasingsByLAux xs ls
    where
      xs = toList p
      ls = IntPartition.toList intPartition

  partitionsIncreasingsByLAux :: [T] -> [Int] -> [[[T]]]
  partitionsIncreasingsByLAux [] []     = [[]]
  partitionsIncreasingsByLAux [] _      = []
  partitionsIncreasingsByLAux _  []     = []
  partitionsIncreasingsByLAux xs (l:ls) = [is:iss |
                   is  <- increasingsByL' xs l,
                   iss <- partitionsIncreasingsByLAux (xs L.\\ is) ls]

  {-|
    'isClassLeader xss' returns 'True' if and only if xss is composed of
    ascending sorted list, each list being sorted ascending.
  -}
  isClassLeader :: [Permutation] -> Bool
  isClassLeader ps = xss == xss'
    where
      xss = [toList p | p <- ps]
      xss' = L.sort . fmap L.sort $ fmap toList ps

  {-|
    'partitionsIncreasings p n' return all partitions of permutation 'p' into 'k'
    increasing subsequences.
  -}
  partitionsIncreasings :: Permutation -> Int -> [[Permutation]]
  partitionsIncreasings p k
    | lenLongestDecreasingSub p > k = []
    | otherwise                     = partitionsIncreasingsAux p k


  partitionsIncreasingsAux :: Permutation -> Int -> [[Permutation]]
  partitionsIncreasingsAux p k = [pPartition |
                                  intPartition <- IntPartition.intPartitionsByL n k,
                                  pPartition   <- partitionsIncreasingsByL p intPartition,
                                  isClassLeader pPartition]
    where
      n = size p

  {-|
    'greedyPartitionIncreasings xs f' return a partition of xs into increasing
    subsequences by repeatedly calling function 'f' on the remaining subsequence.
  -}
  greedyPartitionIncreasings :: (Permutation -> Permutation) -> Permutation -> [Permutation]
  greedyPartitionIncreasings f = g . aux []
    where
      g = L.sortBy (flip compare `Fun.on` size)

      aux acc (Permutation []) = acc
      aux acc p                = aux (q1:acc) q2
        where
          q1  = f p
          q2 = diff p q1

  {-|
    'greedyPartitionIncreasings1' takes a list 'xs'. It greedily computes a partition
    of 'xs' into increasing subsequences.
  -}
  greedyPartitionIncreasings1 :: Permutation -> [Permutation]
  greedyPartitionIncreasings1 = greedyPartitionIncreasings greedyIncreasing1

  {-|
    'greedyIncreasing1' takes a list 'xs'. It greedily computes an increasing
    subsequence of 'xs'.
  -}
  greedyIncreasing1 :: Permutation -> Permutation
  greedyIncreasing1 (Permutation [])     = mkEmpty
  greedyIncreasing1 (Permutation (x:xs)) = fromListUnsafe $ x:greedyIncreasing1Aux x xs

  greedyIncreasing1Aux :: T -> [T] -> [T]
  greedyIncreasing1Aux _ []      = []
  greedyIncreasing1Aux x (x':xs)
    | x' > x    = x':greedyIncreasing1Aux x' xs
    | otherwise =    greedyIncreasing1Aux x  xs

  {-|
    'greedyPartitionIncreasings1' takes a list 'xs'. It greedily computes a partition
    of 'xs' into increasing subsequences.
  -}
  greedyPartitionIncreasings2 :: Permutation -> [Permutation]
  greedyPartitionIncreasings2 = greedyPartitionIncreasings greedyIncreasing2


  {-|
    'greedyIncreasing1' takes a list 'xs'. It greedily computes an increasing
    subsequence of 'xs'.
  -}
  greedyIncreasing2 :: Permutation -> Permutation
  greedyIncreasing2 = longestIncreasingSub

  {-|
    'longestIncreasingSub xs' returns a longest increasing subsequences in 'xs'.
  -}
  longestIncreasingSub :: Permutation -> Permutation
  longestIncreasingSub = post . Patience.longestIncreasing . pre
    where
      is   = [1..] :: [Int]
      pre  = flip L.zip is . toList
      post =  fromListUnsafe . L.map T.fst . L.reverse

  {-|
    'lenLongestIncreasingSub xs' returns the length of the longest increasing
    subsequences in 'xs'.
  -}
  lenLongestIncreasingSub:: Permutation -> Int
  lenLongestIncreasingSub = size . longestIncreasingSub

  {-|
    'longestDecreasingSub xs' returns a longest decreasing subsequences in 'xs'.
  -}
  longestDecreasingSub :: Permutation -> Permutation
  longestDecreasingSub = post . Patience.longestIncreasing . pre
    where
      is   = [1..] :: [Int]
      pre  = flip L.zip is . L.reverse . toList
      post = fromListUnsafe . L.reverse . L.map T.fst . L.reverse

  {-|
    'lenLongestDecreasingSub xs' returns the length of the longest decreasing
    subsequences in 'xs'.
  -}
  lenLongestDecreasingSub :: Permutation -> Int
  lenLongestDecreasingSub = size . longestDecreasingSub

  {-|
    'randPermutation' takes a permutation 'p' and a generator 'g', and
    returns a random permutation of 'p', together with a new generator.
  -}
  randPermutation :: R.RandomGen g => Permutation -> g -> (Permutation, g)
  randPermutation p g = (fromListUnsafe xs, g')
    where
      (xs, g') = flip Random.randPermutation g $ toList p

  {-|
    'randPermutation'' takes an integer 'n' and a generator 'g', and
    returns a random permutation of '[1..n]', together with a new generator.
  -}
  randPermutation' :: R.RandomGen g => Int -> g -> (Permutation, g)
  randPermutation' n g = (p', g')
    where
      p        = mkIncreasing n
      (p', g') = randPermutation p g

  -- {-|
  --   'randKIncreasing' takes two integers 'n' and 'k' and a generator 'g'.
  --   It returns a random permutation of length 'n' that is the union of 'k'
  --   increasings sequences, together with a new generator.
  -- -}
  randKIncreasing :: R.RandomGen g => Int -> Int -> g -> (Permutation, g)
  randKIncreasing n k g =
    if lenLongestDecreasingSub p > k
      then randKIncreasing n k g'
      else (p, g')
    where
      (p, g') = randPermutation' n g

  {-|
    'randKIncreasings' takes three integers 'n', 'k' and 'm' and a generator 'g'.
    It returns 'm' random permutations of length 'n' (each permutation is the
    union of 'k' increasings sequences), together with a new generator.
  -}
  randKIncreasings :: (R.RandomGen g) => Int -> Int -> Int -> g -> ([Permutation], g)
  randKIncreasings n k = aux []
    where
      aux acc 0 g = (acc, g)
      aux acc m g = aux (xs:acc) (m-1) g'
        where
          (xs, g') = randKIncreasing n k g
