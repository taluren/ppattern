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
(
-- * The @Perm@ type
Perm(..)
, fromIntList
, fromList
, fromIntListUnsafe
, fromListUnsafe
, mkEmpty

-- * Conversions
, toList
, toIntList

  -- * Monotonic Perms
, mkIncreasing
, mkDecreasing

  -- * Querying
, size

  ---
, diff

  -- * Enumerating subsequences
-- , increasingsL

  -- * Partitioning
, mkIncreasings
, increasingPartition
, increasingPartitions

  -- * LCS
, longestIncreasing
, longestIncreasingLength
, longestDecreasing
, longestDecreasingLength

  -- * Random
, randPerm
, randPerm'
, randKIncreasing
, randKIncreasings

-- , splits
-- , canonical
-- , standard
-- , isClassRepresentative
)
where

  import qualified Data.Tuple         as T
  import qualified Data.List          as L
  import qualified Data.Foldable      as Foldable
  import qualified Data.Function      as Fun
  import qualified Data.IntMap.Strict as IntMap
  -- import qualified Data.List.Split    as Split
  import qualified System.Random

  import qualified Data.Algorithm.Patience as Patience

  import qualified Data.Algorithm.PPattern.IntPartition as IntPartition
  import qualified Data.Algorithm.PPattern.Random       as Random
  import qualified Data.Algorithm.PPattern.CPoint       as CPoint
  import qualified Data.Algorithm.PPattern.Color        as Color
  import qualified Data.Algorithm.PPattern.Combi        as Combi

  {-| The 'Perm' type encapsulates an optional value.
      A permutation is a list of 'Int'.
  -}
  newtype Perm = Perm [Int] deriving (Eq, Ord, Show, Read)

  {-|
    'mkEmpty' returns the empty permutation (i.e., the Perm with no element).
  -}
  mkEmpty :: Perm
  mkEmpty = fromListUnsafe []

  {-|
    'fromIntList xs' construct a permutation from int list 'xs'.
    Warning: the elements of 'xs' are not reduced.
  -}
  fromIntList :: [Int] -> Perm
  fromIntList = fromList . fmap (\n -> fromIntegral n :: Int)

  {-|
    'fromList xs' construct a reduced permutation from a 'Int' list xs.
  -}
  fromList ::  [Int] -> Perm
  fromList = reduce . fromListUnsafe

  {-|
    'fromList xs' constructs a permutation from the int list 'xs'.
    Warning: the elements of 'xs' are not reduced.
  -}
  fromIntListUnsafe :: [Int] -> Perm
  fromIntListUnsafe = fromListUnsafe . fmap (\n -> fromIntegral n :: Int)

  {-|
    'fromList xs' constructs a permutation from the Int list 'xs'.
    Warning: the elements of 'xs' are not reduced.
  -}
  fromListUnsafe :: [Int] -> Perm
  fromListUnsafe = Perm

  {-|
    Turn a permutation into an int list.
  -}
  toIntList :: Perm -> [Int]
  toIntList (Perm xs) = fmap (\n -> fromIntegral n :: Int) xs

  {-|
    Turn a permutation into a Int. list.
  -}
  toList :: Perm -> [Int]
  toList (Perm xs) = xs

  {-|
    Helper function. Index (from 1) the elements of a list.
  -}
  index :: Perm -> [(Int, Int)]
  index = L.zip [1..] . toList

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
  reduce = fromListUnsafe . extract . sortByIdx . L.zip ([1..] :: [Int]) . sortByVal . L.zip ([1..] :: [Int]) . toList
    where
      sortByVal  = L.sortBy (compare `Fun.on` T.snd)
      sortByIdx  = L.sortBy (compare `Fun.on` (T.fst . T.snd))
      extract    = fmap T.fst

  {-|
    'mkIncreasing n' contructs the increasing permutation 1 2 ... n.
    The function returns the empty permutation if 'n' is non-positive.
  -}
  mkIncreasing :: Int -> Perm
  mkIncreasing n = fromListUnsafe [1..n']
    where
      n' = fromIntegral n :: Int

  {-|
    'mkDecreasing n' contructs the decreasing permutation n n-1 ... 1.
    The function returns the empty permutation if 'n' is non-positive.
  -}
  mkDecreasing :: Int -> Perm
  mkDecreasing n = fromListUnsafe [n',n'-1..1]
    where
      n' = fromIntegral n :: Int

  {-|
    Return the size of a permutation.
  -}
  size :: Perm -> Int
  size = L.length . toList

  {-|
    'diff p q' returns the difference permutation of 'p' and 'q'.
  -}
  diff :: Perm -> Perm -> Perm
  diff (Perm xs) (Perm ys) = fromListUnsafe (xs L.\\ ys)



  {-|
    'longestIncreasing xs' returns a longest increasing subsequences in 'xs'.
  -}
  longestIncreasing :: Perm -> [Int]
  longestIncreasing p = L.reverse . fmap T.fst $ Patience.longestIncreasing xs
    where
      ints = [1..] :: [Int]
      xs   = flip L.zip ints $ toIntList p

  {-|
    'longestIncreasingLength xs' returns the length of the longest increasing
    subsequences in 'xs'.
  -}
  longestIncreasingLength:: Perm -> Int
  longestIncreasingLength = L.length . longestIncreasing

  {-|
    'longestDecreasing xs' returns a longest decreasing subsequences in 'xs'.
  -}
  longestDecreasing :: Perm -> [Int]
  longestDecreasing p = fmap T.fst $ Patience.longestIncreasing xs
    where
      ints = [1..] :: [Int]
      xs   = flip L.zip ints . L.reverse $ toIntList p

  {-|
    'longestDecreasingLength xs' returns the length of the longest decreasing
    subsequences in 'xs'.
  -}
  longestDecreasingLength :: Perm -> Int
  longestDecreasingLength = L.length . longestDecreasing

  {-|
    'randPerm' takes a permutation 'p' and a generator 'g', and
    returns a random permutation of 'p', together with a new generatoRandom.
  -}
  randPerm :: System.Random.RandomGen g => Perm -> g -> (Perm, g)
  randPerm p g = (fromListUnsafe xs, g')
    where
      (xs, g') = flip Random.randPerm g $ toList p

  {-|
    'randPerm'' takes an integer 'n' and a generator 'g', and
    returns a random permutation of '[1..n]', together with a new generatoRandom.
  -}
  randPerm' :: System.Random.RandomGen g => Int -> g -> (Perm, g)
  randPerm' n g = (fromList xs, g')
    where
      (xs, g') = Random.randPerm [1..n] g

  -- 'mkIncreasings xs' constructs increasing lists, where the length of each
  -- list is given by the elements of 'xs'.
  mkIncreasings :: System.Random.RandomGen g => [Int] -> g -> ([[Int]], g)
  mkIncreasings ls = mkIncreasingsAux [] [1..n] ls
    where
      n = Foldable.sum ls

  mkIncreasingsAux :: System.Random.RandomGen g => [[Int]] -> [Int] -> [Int] -> g -> ([[Int]], g)
  mkIncreasingsAux acc _  []       g = (acc, g)
  mkIncreasingsAux acc xs (l : ls) g = mkIncreasingsAux (ys : acc) (xs L.\\ ys) ls g'
    where
      (ys, g') = Random.sample l xs g

  -- {-|
  --   'randKIncreasing' takes two integers 'n' and 'k' and a generator 'g'.
  --   It returns a random permutation of length 'n' that is the union of 'k'
  --   increasings sequences, together with a new generatoRandom.
  -- -}
  randKIncreasing :: System.Random.RandomGen g => Int -> Int -> g -> (Perm, g)
  randKIncreasing n k g
    | k > n     = (mkEmpty, g)
    | otherwise = (p, g''')
    where
      -- rand int partition
      (intPartition, g') = IntPartition.randIntPartition n k g
      partitionAsList = IntPartition.toList intPartition
      (partitionAsIncreasingLists, g'') = mkIncreasings partitionAsList g'

      -- random shuffle
      (xs, g''') = Random.randShuffle partitionAsIncreasingLists g''

      -- make permutation
      p = fromList xs

  {-|
    'randKIncreasings' takes three integers 'n', 'k' and 'm' and a generator 'g'.
    It returns 'm' random permutations of length 'n' (each Perm is the
    union of 'k' increasings sequences), together with a new generatoRandom.
  -}
  randKIncreasings :: (System.Random.RandomGen g) => Int -> Int -> Int -> g -> ([Perm], g)
  randKIncreasings n k = aux []
    where
      aux acc 0 g = (acc, g)
      aux acc m g = aux (xs:acc) (m-1) g'
        where
          (xs, g') = randKIncreasing n k g
