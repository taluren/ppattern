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
  import qualified System.Random

  import qualified Data.Algorithm.Patience as Patience

  import qualified Data.Algorithm.PPattern.Types        as T
  import qualified Data.Algorithm.PPattern.IntPartition as IntPartition
  import qualified Data.Algorithm.PPattern.Random       as Random
  import qualified Data.Algorithm.PPattern.CPoint       as CPoint
  import qualified Data.Algorithm.PPattern.Color        as Color
  import qualified Data.Algorithm.PPattern.Combi        as Combi

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

  -- {-|
  --   'increasingsL p k' return the list of all increasing subsequences
  --   of length 'k' of the permutation 'p'.
  -- -}
  -- increasingsL :: Perm ->  Int -> [Perm]
  -- increasingsL (Perm xs) k = fmap fromListUnsafe xss
  --   where
  --     xss = Splitting.increasingsL xs k


  -- Make an initial list of colored points. Each element from the longest
  -- decreasing subsequence is given a distinct colors. All other elements
  -- are given the 'not determined yet' color 0.
  mkCPoints :: [(Int, T.T)] -> [T.T] -> [Color.Color] -> [(Int, T.T, Color.Color)]
  mkCPoints []             _            _         = []
  mkCPoints ((i, x) : ixs) []           refColors = (i, x, 0) : mkCPoints ixs [] refColors
  mkCPoints ((_, _) : _)   _            []        = error "We shouldn't be there"
  mkCPoints ((i, x) : ixs) ys'@(y : ys) refColors'@(c : refColors)
    | x == y    = (i, x, c) : mkCPoints ixs ys  refColors
    | otherwise = (i, x, 0) : mkCPoints ixs ys' refColors'

  {-|
    'increasingPartition p' returns a partition of the permutation 'p' into
    'l' increasing subsequences, wjere 'l' is the length of a longest increasing
    subsequence of 'p'.
  -}
  increasingPartition :: Perm -> [CPoint.CPoint]
  increasingPartition p = increasingPartitionAux cps cs prevMap nextMap
    where
      -- Index permutation p.
      indexed = index p

      -- A longest decreasing subsequence of p.
      decreasing = longestDecreasing p

      -- The length oa longest decreasing subsequence of p.
      l = L.length decreasing

      -- colours
      cs = [1..l]

      -- p as a list of coloured points (the lements of the decreasing
      -- subsequence get distinct colours)
      cps = mkCPoints indexed decreasing cs

      -- maps
      prevMap = IntMap.empty
      nextMap = IntMap.fromList $ L.zip [1..l] decreasing

  increasingPartitionAux :: [(Int, T.T, Color.Color)] -> [Color.Color] -> IntMap.IntMap T.T -> IntMap.IntMap T.T -> [CPoint.CPoint]
  increasingPartitionAux []                 _  _       _       = []
  increasingPartitionAux ((i, x, 0) : ixcs) cs prevMap nextMap = increasingPartitionAux1  (i, x)    ixcs cs prevMap nextMap
  increasingPartitionAux ((i, x, c) : ixcs) cs prevMap nextMap = increasingPartitionAux2 (i, x, c) ixcs cs prevMap nextMap

  -- increasingPartitionAux for 0-coloured points
  -- Find any compatible colour.
  increasingPartitionAux1 :: (T.T, T.T)-> [(Int, T.T, Color.Color)] -> [Color.Color] -> IntMap.IntMap T.T -> IntMap.IntMap T.T -> [CPoint.CPoint]
  increasingPartitionAux1 is ixcs cs = increasingPartitionAux1' is ixcs cs cs

  -- increasingPartitionAux for 0-coloured points
  -- Find any compatible colour.
  increasingPartitionAux1' :: (T.T, T.T)-> [(Int, T.T, Color.Color)] -> [Color.Color] -> [Color.Color] -> IntMap.IntMap T.T -> IntMap.IntMap T.T -> [CPoint.CPoint]
  increasingPartitionAux1' _      _    []       _   _       _      = error "We shouldn't be there"
  increasingPartitionAux1' (i, x) ixcs (c : cs) cs' prevMap nextMap
    | prevTest && nextTest = cp : increasingPartitionAux ixcs cs' prevMap' nextMap'
    | otherwise            = increasingPartitionAux1' (i, x) ixcs cs cs' prevMap nextMap
    where
      prevTest = agreeWithPrevElement x c prevMap
      nextTest = agreeWithNextElement x c nextMap
      cp       = CPoint.mkCPoint i x c
      prevMap' = updatePrevMap x c prevMap
      nextMap' = updateNextMap x c nextMap

  {-|
    increasingPartitionsAux for colored points (those point that are part of
    the selected longest decreasing subsequence).
  -}
  increasingPartitionAux2 :: (T.T, T.T, IntMap.Key) -> [(Int, T.T, Color.Color)] -> [Color.Color] -> IntMap.IntMap T.T -> IntMap.IntMap T.T  -> [CPoint.CPoint]
  increasingPartitionAux2 (i, x, c) ixcs cs prevMap nextMap = cp : cps
    where
      -- Nex color point
      cp  = CPoint.mkCPoint i x c

      -- This color point in now a left constraint
      prevMap' = updatePrevMap x c prevMap

      -- This color point in now a left constraint
      nextMap' = updateNextMap x c nextMap

      -- recursively compute the remaining colored points
      cps = increasingPartitionAux ixcs cs prevMap' nextMap'

  {-|
    'increasingPartitions p k' returns all partitions of the permutation 'p' into
    'k' increasing subsequences.
  -}
  increasingPartitions :: Perm -> Int -> [[CPoint.CPoint]]
  increasingPartitions p k
    | l > k     = []
    | otherwise = Foldable.concat [increasingPartitionsAux cps cs prevMap nextMap
                                  | refColors     <- [1..k] `Combi.choose` l
                                  , permRefColors <- L.permutations refColors
                                  , let cps     = mkCPoints indexed decreasing permRefColors
                                  , let prevMap = IntMap.empty
                                  , let nextMap = IntMap.fromList $ L.zip permRefColors decreasing]
    where
      -- A longest decreasing subsequence of p.
      decreasing = longestDecreasing p

      -- The length oa longest decreasing subsequence of p.
      l = L.length decreasing

      -- The list of available colors.
      cs = [1..k]

      -- Index permutation p.
      indexed = index p

  increasingPartitionsAux :: [(Int, T.T, Color.Color)] -> [Color.Color] -> IntMap.IntMap T.T -> IntMap.IntMap T.T -> [[CPoint.CPoint]]
  increasingPartitionsAux []                 _  _       _       = [[]]
  increasingPartitionsAux ((i, x, 0) : ixcs) cs prevMap nextMap = increasingPartitionsAux1  (i, x)    ixcs cs prevMap nextMap
  increasingPartitionsAux ((i, x, c) : ixcs) cs prevMap nextMap = increasingPartitionsAux2 (i, x, c) ixcs cs prevMap nextMap

  {-|
    increasingPartitionsAux for color-0 points (those point that are not part of
    the selected longest decreasing subsequence).
  -}
  increasingPartitionsAux1 :: (T.T, T.T)-> [(Int, T.T, Color.Color)] -> [Color.Color] -> IntMap.IntMap T.T -> IntMap.IntMap T.T -> [[CPoint.CPoint]]
  increasingPartitionsAux1 (i, x) ixcs cs prevMap nextMap = Foldable.concat cpsss
    where
      cpsss = [fmap (cp :) cpss | c <- cs
                                , agreeWithPrevElement x c prevMap
                                , agreeWithNextElement x c nextMap
                                , let cp       = CPoint.mkCPoint i x c
                                , let prevMap' = updatePrevMap x c prevMap
                                , let nextMap' = updateNextMap x c nextMap
                                , let cpss     = increasingPartitionsAux ixcs cs prevMap' nextMap']

  {-|
    increasingPartitionsAux for colored points (those point that are part of
    the selected longest decreasing subsequence).
  -}
  increasingPartitionsAux2 :: (T.T, T.T, IntMap.Key) -> [(Int, T.T, Color.Color)] -> [Color.Color] -> IntMap.IntMap T.T -> IntMap.IntMap T.T  -> [[CPoint.CPoint]]
  increasingPartitionsAux2 (i, x, c) ixcs cs prevMap nextMap = fmap (cp :) cpss
    where
      -- Nex color point
      cp  = CPoint.mkCPoint i x c

      -- This color point in now a left constraint
      prevMap' = updatePrevMap x c prevMap

      -- This color point in now a left constraint
      nextMap' = updateNextMap x c nextMap

      -- recursively compute the remaining colored points
      cpss = increasingPartitionsAux ixcs cs prevMap' nextMap'


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
  randPerm' n g = (p', g')
    where
      p        = mkIncreasing n
      (p', g') = Random.randPerm p g

  -- 'mkIncreasings xs' constructs increasing lists, where the length of each
  -- list is given by the elements of 'xs'.
  mkIncreasings :: System.Random.RandomGen g => [Int] -> g -> ([[Int]], g)
  mkIncreasings xs = Split.splitPlaces ls xs
    where
      n = Foldable.sum xs
      (ls', g') = Random.randPerm n g

  -- {-|
  --   'randKIncreasing' takes two integers 'n' and 'k' and a generator 'g'.
  --   It returns a random permutation of length 'n' that is the union of 'k'
  --   increasings sequences, together with a new generatoRandom.
  -- -}
  randKIncreasing :: System.Random.RandomGen g => Int -> Int -> g -> (Perm, g)
  randKIncreasing n k g
    | k > n     = (mkEmpty, g)
    | otherwise = (p, g'')
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
