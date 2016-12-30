{-|
Module      : Data.Algorithm.PPattern.Perm
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Perm
-- (
--   -- * The @Perm@ type
--   Perm(..)
-- , fromIntList
-- , fromList
-- , fromIntListUnsafe
-- , fromListUnsafe
-- , mkEmpty
-- , reduce
--
--   -- * Monotonic Perms
-- , mkIncreasing
-- , mkDecreasing
--
--   -- * Conversions
-- , toList
-- , toIntList
--
--   -- * Querying
-- , size
--
--   ---
-- , diff
--
--   -- * Enumerating subsequences
-- , increasingsL
--
--   -- * Partitioning
-- , partitionsIncreasings
-- , greedyIncreasing1
-- , greedyPartitionIncreasings1
-- , greedyIncreasing2
-- , greedyPartitionIncreasings2
--
--   -- * LCS
-- , longestIncreasingSub
-- , lenLongestIncreasingSub
-- , longestDecreasingSub
-- , lenLongestDecreasingSub
--
--   -- * Random
-- , randPerm
-- , randPerm'
-- , randKIncreasing
-- , randKIncreasings
-- )
where

  import qualified Data.List     as L
  import qualified Data.Foldable as Foldable
  import qualified Data.Monoid   as Monoid
  import qualified Data.Function as Fun
  import qualified System.Random as R

  import qualified Data.Algorithm.Patience as Patience

  import qualified Data.Algorithm.PPattern.Types        as T
  import qualified Data.Algorithm.PPattern.Random       as Random
  import qualified Data.Algorithm.PPattern.IntPartition as IntPartition

  newtype Perm = Perm [T.T] deriving (Eq, Ord, Show, Read)

  {-|
    'fromIntList xs' construct a reduced permutation from int list xs.
  -}
  fromIntList :: [Int] -> Perm
  fromIntList = fromList . fmap (\n -> fromIntegral n :: T.T)

  {-|
    'fromList xs' construct a reduced permutation from T.T list xs.
  -}
  fromList ::  [T.T] -> Perm
  fromList = reduce . fromListUnsafe

  {-|
    'fromList xs' constructs a permutation from the int list 'xs'.
    Warning: the elements of 'xs' are not reduced.
  -}
  fromIntListUnsafe :: [Int] -> Perm
  fromIntListUnsafe = fromListUnsafe . fmap (\n -> fromIntegral n :: T.T)

  {-|
    'fromList xs' constructs a permutation from the T.T list 'xs'.
    Warning: the elements of 'xs' are not reduced.
  -}
  fromListUnsafe :: [T.T] -> Perm
  fromListUnsafe = Perm

  {-|
    'mkEmpty' returns the empty permutation (i.e., the Perm with no element).
  -}
  mkEmpty :: Perm
  mkEmpty = fromListUnsafe []

  {-|
    'mkIncreasing n' contructs the increasing permutation 1 2 ... n.
    The function returns the empty permutation if 'n' is non-positive.
  -}
  mkIncreasing :: Int -> Perm
  mkIncreasing n = fromListUnsafe [1..n']
    where
      n' = fromIntegral n :: T.T

  {-|
    'mkDecreasing n' contructs the decreasing permutation n n-1 ... 1.
    The function returns the empty permutation if 'n' is non-positive.
  -}
  mkDecreasing :: Int -> Perm
  mkDecreasing n = fromListUnsafe [n',n'-1..1]
    where
      n' = fromIntegral n :: T.T

  {-|
    Turn a permutation into an int list.
  -}
  toIntList :: Perm -> [Int]
  toIntList (Perm xs) = fmap (\n -> fromIntegral n :: Int) xs

  {-|
    Turn a permutation into a T.T. list.
  -}
  toList :: Perm -> [T.T]
  toList (Perm xs) = xs

  {-|
    Helper function. Index (from 1) the elements of a list
  -}
  index :: [a] -> [(T.T, a)]
  index = L.zip ([1..] :: [T.T])

  {-|
    'reduce p' returns the reduced form of the permutation 'p'.

    λ: reduce (Perm [])
    Perm []
    λ: reduce (Perm [1..5])
    Perm [1,2,3,4,5]
    λ: reduce (Perm [5,9,2,7,3])
    Perm [3,5,1,4,2]
  -}
  reduce :: Perm -> Perm
  reduce = fromListUnsafe . getElts . sortByIdx . index . sortByVal . index . toList
    where
      sortByVal  = L.sortBy (compare `Fun.on` snd)
      sortByIdx  = L.sortBy (compare `Fun.on` (fst . snd))
      getElts    = fmap fst

  {-|
    Return the size of a permutation.È of the Perm 'p'.
  -}
  size :: Perm -> T.Length
  size = L.length . toList

  {-|
    'diff p q' returns the difference permutation of 'p' and 'q'.
  -}
  diff :: Perm -> Perm -> Perm
  diff (Perm xs) (Perm ys) = fromListUnsafe (xs L.\\ ys)

  {-|
    Helper function.
    'increasingsL' xs k' returns the list of all increasing subsequences
    of length 'k' of the T.T list 'xs'.
  -}
  increasingsL' :: [T.T] ->  Int -> [[T.T]]
  increasingsL' [] _ = []
  increasingsL' xs k = increasingsLAux' xs k k (L.head xs)

  increasingsLAux' ::  [T.T] ->  Int -> Int -> T.T -> [[T.T]]
  increasingsLAux' _      _ 0 _  = [[]]
  increasingsLAux' []     _ _ _  = []
  increasingsLAux' (x:xs) k k' x'
    | k == k' || x > x' = fmap (x:) xss `Monoid.mappend` increasingsLAux' xs k k' x'
    | otherwise         = increasingsLAux' xs k k' x'
    where
      xss = increasingsLAux' xs k (k'-1) x

  {-|
    'increasingsL p k' return the list of all increasing subsequences
    of length 'k' of the permutation 'p'.
  -}
  increasingsL :: Perm ->  Int -> [Perm]
  increasingsL (Perm xs) k = fmap fromListUnsafe xss
    where
      xss = increasingsL' xs k

  -- {-|
  --   'partitionsIncreasingsL p ks' returns all partitions of the Permutation
  --   'p' into /|ks|/ increasing subsequences of length ks = [k1, k2, ..., kp].
  -- -}
  -- partitionsIncreasingsL :: Perm -> IntPartition.IntPartition -> [[Perm]]
  -- partitionsIncreasingsL p intPartition = fmap fromListUnsafe <$> partitions
  --   where
  --     xs         = toList p
  --     ls         = IntPartition.toList intPartition
  --     partitions = partitionsIncreasingsLAux xs ls
  --
  -- partitionsIncreasingsLAux :: [T.T] -> [Int] -> [[[T.T]]]
  -- partitionsIncreasingsLAux [] []     = [[]]
  -- partitionsIncreasingsLAux [] _      = []
  -- partitionsIncreasingsLAux _  []     = []
  -- partitionsIncreasingsLAux xs (l:ls) = [is:iss |
  --                                          is  <- increasingsL' xs l,
  --                                          iss <- partitionsIncreasingsLAux (xs L.\\ is) ls]

  {-|
   Helper function.
  -}
  canonical :: [Perm] -> [Perm]
  canonical = concatSort . unindex . groupBySize . sortBySize . indexBySize
   where
     indexBySize = fmap (\p -> (size p, p))
     sortBySize  = L.sortBy (compare `Fun.on` fst)
     groupBySize = L.groupBy ((==) `Fun.on` fst)
     unindex     = fmap (fmap snd)
     concatSort  = Foldable.concatMap L.sort

  {-|
   Helper function.
  -}
  isClassRepresentative :: [Perm] -> Bool
  isClassRepresentative ps = ps == canonical ps

  {-|
    'partitionsIncreasings p k' return all partitions of the permutation 'p' into
    'k' increasing subsequences.
  -}
  partitionsIncreasings :: Perm -> Int -> [[Perm]]
  partitionsIncreasings p k
    | lenLongestDecreasingSub p > k = []
    | otherwise                     = partitionsIncreasingsAux p (size p) k


  partitionsIncreasingsAux :: Perm -> T.Length -> Int -> [[Perm]]
  partitionsIncreasingsAux p n k =
    [ps | ps <- Splitting.split p (IntPartition.partitionsL n k),
          isClassRepresentative ps]


  {-|
    'greedyPartitionIncreasings xs f' return a partition of xs into increasing
    subsequences by repeatedly calling function 'f' on the remaining subsequence.
  -}
  greedyPartitionIncreasings :: (Perm -> Perm) -> Perm -> [Perm]
  greedyPartitionIncreasings f = g . aux []
    where
      g = L.sortBy (flip compare `Fun.on` size)

      aux acc (Perm []) = acc
      aux acc p                = aux (q1:acc) q2
        where
          q1  = f p
          q2 = diff p q1

  {-|
    'greedyPartitionIncreasings1' takes a list 'xs'. It greedily computes a partition
    of 'xs' into increasing subsequences.
  -}
  greedyPartitionIncreasings1 :: Perm -> [Perm]
  greedyPartitionIncreasings1 = greedyPartitionIncreasings greedyIncreasing1

  {-|
    'greedyIncreasing1' takes a list 'xs'. It greedily computes an increasing
    subsequence of 'xs'.
  -}
  greedyIncreasing1 :: Perm -> Perm
  greedyIncreasing1 (Perm [])     = mkEmpty
  greedyIncreasing1 (Perm (x:xs)) = fromListUnsafe $ x:greedyIncreasing1Aux x xs

  greedyIncreasing1Aux :: T.T -> [T.T] -> [T.T]
  greedyIncreasing1Aux _ []      = []
  greedyIncreasing1Aux x (x':xs)
    | x' > x    = x':greedyIncreasing1Aux x' xs
    | otherwise =    greedyIncreasing1Aux x  xs

  {-|
    'greedyPartitionIncreasings1' takes a list 'xs'. It greedily computes a partition
    of 'xs' into increasing subsequences.
  -}
  greedyPartitionIncreasings2 :: Perm -> [Perm]
  greedyPartitionIncreasings2 = greedyPartitionIncreasings greedyIncreasing2


  {-|
    'greedyIncreasing1' takes a list 'xs'. It greedily computes an increasing
    subsequence of 'xs'.
  -}
  greedyIncreasing2 :: Perm -> Perm
  greedyIncreasing2 = longestIncreasingSub

  {-|
    'longestIncreasingSub xs' returns a longest increasing subsequences in 'xs'.
  -}
  longestIncreasingSub :: Perm -> Perm
  longestIncreasingSub = post . Patience.longestIncreasing . pre
    where
      is   = [1..] :: [Int]
      pre  = flip L.zip is . toIntList
      post =  fromIntListUnsafe . L.map fst . L.reverse

  {-|
    'lenLongestIncreasingSub xs' returns the length of the longest increasing
    subsequences in 'xs'.
  -}
  lenLongestIncreasingSub:: Perm -> Int
  lenLongestIncreasingSub = size . longestIncreasingSub

  {-|
    'longestDecreasingSub xs' returns a longest decreasing subsequences in 'xs'.
  -}
  longestDecreasingSub :: Perm -> Perm
  longestDecreasingSub = post . Patience.longestIncreasing . pre
    where
      is   = [1..] :: [Int]
      pre  = flip L.zip is . L.reverse . toIntList
      post = fromIntListUnsafe . L.reverse . L.map fst . L.reverse

  {-|
    'lenLongestDecreasingSub xs' returns the length of the longest decreasing
    subsequences in 'xs'.
  -}
  lenLongestDecreasingSub :: Perm -> Int
  lenLongestDecreasingSub = size . longestDecreasingSub

  {-|
    'randPerm' takes a permutation 'p' and a generator 'g', and
    returns a random permutation of 'p', together with a new generator.
  -}
  randPerm :: R.RandomGen g => Perm -> g -> (Perm, g)
  randPerm p g = (fromListUnsafe xs, g')
    where
      (xs, g') = flip Random.randPerm g $ toList p

  {-|
    'randPerm'' takes an integer 'n' and a generator 'g', and
    returns a random permutation of '[1..n]', together with a new generator.
  -}
  randPerm' :: R.RandomGen g => Int -> g -> (Perm, g)
  randPerm' n g = (p', g')
    where
      p        = mkIncreasing n
      (p', g') = randPerm p g

  -- {-|
  --   'randKIncreasing' takes two integers 'n' and 'k' and a generator 'g'.
  --   It returns a random permutation of length 'n' that is the union of 'k'
  --   increasings sequences, together with a new generator.
  -- -}
  randKIncreasing :: R.RandomGen g => Int -> Int -> g -> (Perm, g)
  randKIncreasing n k g =
    if lenLongestDecreasingSub p > k
      then randKIncreasing n k g'
      else (p, g')
    where
      (p, g') = randPerm' n g

  {-|
    'randKIncreasings' takes three integers 'n', 'k' and 'm' and a generator 'g'.
    It returns 'm' random permutations of length 'n' (each Perm is the
    union of 'k' increasings sequences), together with a new generator.
  -}
  randKIncreasings :: (R.RandomGen g) => Int -> Int -> Int -> g -> ([Perm], g)
  randKIncreasings n k = aux []
    where
      aux acc 0 g = (acc, g)
      aux acc m g = aux (xs:acc) (m-1) g'
        where
          (xs, g') = randKIncreasing n k g