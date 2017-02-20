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
, reduce

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
, increasingsL

  -- * Partitioning
, partitionsIncreasings
, greedyIncreasing1
, greedyPartitionIncreasings1
, greedyIncreasing2
, greedyPartitionIncreasings2

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
  import qualified System.Random

  import qualified Data.Algorithm.Patience as Patience

  import qualified Data.Algorithm.PPattern.Types        as T
  import qualified Data.Algorithm.PPattern.Splitting    as Splitting
  import qualified Data.Algorithm.PPattern.Random       as Random
  import qualified Data.Algorithm.PPattern.CPoint       as CPoint
  import qualified Data.Algorithm.PPattern.Color        as Color

  {-| The 'Perm' type encapsulates an optional value.
      A permutation is a list of 'T.T'.
  -}
  newtype Perm = Perm [T.T] deriving (Eq, Ord, Show, Read)

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
  fromIntList = fromList . fmap (\n -> fromIntegral n :: T.T)

  {-|
    'fromList xs' construct a reduced permutation from a 'T.T' list xs.
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
    Helper function. Index (from 1) the elements of a list.
  -}
  index :: Perm -> [(Int, T.T)]
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
  reduce = fromListUnsafe . extract . sortByIdx . L.zip ([1..] :: [T.T]) . sortByVal . L.zip ([1..] :: [T.T]) . toList
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
    Return the size of a permutation.
  -}
  size :: Perm -> T.Length
  size = L.length . toList

  {-|
    'diff p q' returns the difference permutation of 'p' and 'q'.
  -}
  diff :: Perm -> Perm -> Perm
  diff (Perm xs) (Perm ys) = fromListUnsafe (xs L.\\ ys)

  {-|
    'increasingsL p k' return the list of all increasing subsequences
    of length 'k' of the permutation 'p'.
  -}
  increasingsL :: Perm ->  Int -> [Perm]
  increasingsL (Perm xs) k = fmap fromListUnsafe xss
    where
      xss = Splitting.increasingsL xs k

  -- {-|
  --  Helper function.
  -- -}
  -- standard :: [Perm] -> [Perm]
  -- standard = unindex . sortBySize . indexBySize
  --  where
  --    indexBySize = fmap (\p -> (size p, p))
  --    sortBySize  = L.sortBy (compare `Fun.on` T.fst)
  --    unindex     = fmap T.snd
  --
  -- {-|
  --  Helper function.
  -- -}
  -- canonical :: [Perm] -> [Perm]
  -- canonical = concatSort . unindex . groupBySize . sortBySize . indexBySize
  --  where
  --    indexBySize = fmap (\p -> (size p, p))
  --    sortBySize  = L.sortBy (compare `Fun.on` T.fst)
  --    groupBySize = L.groupBy ((==) `Fun.on` T.fst)
  --    unindex     = fmap (fmap T.snd)
  --    concatSort  = Foldable.concatMap L.sort
  --
  -- {-|
  --  Helper function.
  -- -}
  -- isClassRepresentative :: [Perm] -> Bool
  -- isClassRepresentative ps = standard ps == canonical ps
  --
  -- {-|
  --   'splits p partitions' takes a permutation 'p' and a list of integer
  --   partitions 'partitions' of some integers.
  -- -}
  -- splits :: Perm -> [IntPartition.IntPartition] -> [[Perm]]
  -- splits p partitions = fmap fromListUnsafe <$> Splitting.splits xs partitions
  --   where
  --     xs = toList p

  -- {-|
  --   'partitionsIncreasings p k' returns all partitions of the permutation 'p' into
  --   'k' increasing subsequences.
  -- -}
  -- partitionsIncreasings :: Perm -> Int -> [[Perm]]
  -- partitionsIncreasings p k
  --   | longestDecreasingLength p > k = []
  --   | otherwise                     = partitionsIncreasingsAux p (size p) k

  {-|
    'partitionsIncreasings p k' returns all partitions of the permutation 'p' into
    'k' increasing subsequences.
  -}
  partitionsIncreasings :: Perm -> Int -> [[CPoint.CPoint]]
  partitionsIncreasings p k
    | l > k     = []
    | otherwise = partitionsIncreasingsAux cps cs prevMap nextMap
    where
      -- A longest decreasing subsequence of p.
      decreasing = longestDecreasing p

      -- The length oa longest decreasing subsequence of p.
      l = L.length decreasing

      -- The list of available colors.
      cs = [1..l]

      -- Index permutation p.
      indexed = index p

      -- Make an initial list of colored points. Each element from the longest
      -- decreasing subsequence is given a distinct colors. All other elements
      -- are given the 'not determined yet' color 0.
      cps = mkCPoints indexed decreasing 1
      mkCPoints []             _            _ = []
      mkCPoints ((i, x) : ixs) []           c = (i, x, 0) : mkCPoints ixs [] c
      mkCPoints ((i, x) : ixs) ys'@(y : ys) c
        | x == y    = (i, x, c) : mkCPoints ixs ys  (c+1)
        | otherwise = (i, x, 0) : mkCPoints ixs ys' c

      -- Previous map constraints
      prevMap = IntMap.empty

      -- Next map constraints
      nextMap = IntMap.fromList $ L.zip [1..] decreasing

  partitionsIncreasingsAux :: [(Int, T.T, Color.Color)] -> [Color.Color] -> IntMap.IntMap T.T -> IntMap.IntMap T.T -> [[CPoint.CPoint]]
  partitionsIncreasingsAux []                 _  _       _       = [[]]
  partitionsIncreasingsAux ((i, x, 0) : ixcs) cs prevMap nextMap = partitionsIncreasingsAux'  (i, x)    ixcs cs prevMap nextMap
  partitionsIncreasingsAux ((i, x, c) : ixcs) cs prevMap nextMap = partitionsIncreasingsAux'' (i, x, c) ixcs cs prevMap nextMap

  partitionsIncreasingsAux' :: (T.T, T.T)-> [(Int, T.T, Color.Color)] -> [Color.Color] -> IntMap.IntMap T.T -> IntMap.IntMap T.T -> [[CPoint.CPoint]]
  partitionsIncreasingsAux' (i, x) ixcs cs prevMap nextMap = Foldable.concat cpsss
    where
      cpsss = [fmap (cp :) cpss | c <- cs
                                , agreeWithPrevElement x c prevMap
                                , agreeWithNextElement x c nextMap
                                , let cp       = CPoint.mkCPoint i x c
                                , let prevMap' = updatePrevMap x c prevMap
                                , let nextMap' = updateNextMap x c nextMap
                                , let cpss     = partitionsIncreasingsAux ixcs cs prevMap' nextMap']

  partitionsIncreasingsAux'' :: (T.T, T.T, IntMap.Key) -> [(Int, T.T, Color.Color)] -> [Color.Color] -> IntMap.IntMap T.T -> IntMap.IntMap T.T  -> [[CPoint.CPoint]]
  partitionsIncreasingsAux'' (i, x, c) ixcs cs prevMap nextMap = fmap (cp :) cpss
    where
      -- Nex color point
      cp  = CPoint.mkCPoint i x c

      -- This color point in now a left constraint
      prevMap' = updatePrevMap x c prevMap

      -- recursively compute the remaining colored points
      cpss = partitionsIncreasingsAux ixcs cs prevMap' nextMap

  agreeWithPrevElement :: Color.Color -> IntMap.Key -> IntMap.IntMap T.T -> Bool
  agreeWithPrevElement x c m = case IntMap.lookup c m of
                                 Nothing -> True
                                 Just x' -> x' < x

  agreeWithNextElement :: Color.Color -> IntMap.Key -> IntMap.IntMap T.T -> Bool
  agreeWithNextElement x c m = case IntMap.lookup c m of
                                 Nothing -> True
                                 Just x' -> x < x'

  updatePrevMap :: Color.Color -> IntMap.Key -> IntMap.IntMap T.T -> IntMap.IntMap T.T
  updatePrevMap x c m = case IntMap.lookup c m of
                          Nothing -> IntMap.insert c x m
                          Just _  -> IntMap.update (\_ -> Just x) c m

  updateNextMap :: Color.Color -> IntMap.Key -> IntMap.IntMap T.T -> IntMap.IntMap T.T
  updateNextMap x c m = case IntMap.lookup c m of
                          Nothing       -> m
                          Just x'
                            | x < x'    -> m
                            | x == x'   -> IntMap.delete c m
                            | otherwise -> error "We shouldn't be there 2" -- make ghc -Werror happy

  {-|
    'greedyPartitionIncreasings xs f' return a partition of xs into increasing
    subsequences by repeatedly calling function 'f' on the remaining subsequence.
  -}
  greedyPartitionIncreasings :: (Perm -> Perm) -> Perm -> [Perm]
  greedyPartitionIncreasings f = g . aux []
    where
      g = L.sortBy (flip compare `Fun.on` size)

      aux acc (Perm []) = acc
      aux acc p                = aux (q1 : acc) q2
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
  greedyIncreasing2 = fromList . longestIncreasing

  {-|
    'longestIncreasing xs' returns a longest increasing subsequences in 'xs'.
  -}
  longestIncreasing :: Perm -> [T.T]
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
  longestDecreasing :: Perm -> [T.T]
  longestDecreasing p = L.reverse . fmap T.fst $ Patience.longestIncreasing xs
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
  randPerm' n g = (p', g')
    where
      p        = mkIncreasing n
      (p', g') = randPerm p g

  -- {-|
  --   'randKIncreasing' takes two integers 'n' and 'k' and a generator 'g'.
  --   It returns a random permutation of length 'n' that is the union of 'k'
  --   increasings sequences, together with a new generatoRandom.
  -- -}
  randKIncreasing :: System.Random.RandomGen g => Int -> Int -> g -> (Perm, g)
  randKIncreasing n k g =
    if longestDecreasingLength p > k
      then randKIncreasing n k g'
      else (p, g')
    where
      (p, g') = randPerm' n g

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
