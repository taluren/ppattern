{-|
Module      : Data.Algorithm.PPattern.Splitting
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Splitting
-- (
--  splits
-- )
where

  import qualified Data.List      as L
  import qualified Data.Foldable      as Foldable
  import qualified Data.IntMap.Strict as IntMap
  import qualified Data.Monoid        as Monoid

  import qualified Data.Algorithm.PPattern.IntPartition as IntPartition

  newtype Tree = Tree { children :: IntMap.IntMap Tree }
                 deriving (Show)

  {-|
    Construct an empty tree.
  -}
  emptyTree :: Tree
  emptyTree = Tree { children=IntMap.empty }

  {-|
    Search for a intger key in a tree.
  -}
  lookupTree :: Int -> Tree -> Maybe Tree
  lookupTree x Tree { children=m } = IntMap.lookup x m

  {-|
    Insert a list of integers in a tree.
  -}
  insertTree :: [Int] -> Tree -> Tree
  insertTree []     t                   = t
  insertTree (x:xs) Tree { children=m } = case IntMap.lookup x m of
    Nothing -> Tree { children=IntMap.insert x (path xs)                      m }
    Just t  -> Tree { children=IntMap.update (\_ -> Just (insertTree xs t)) x m }

  {-|
    Construct a path-tree. Helper function for function 'insertTree'.
  -}
  path :: [Int] -> Tree
  path = Foldable.foldr (\x t -> Tree { children=IntMap.singleton x t }) emptyTree

  {-|
    Make a tree from a list of list of integers.
  -}
  mkTree :: [[Int]] -> Tree
  mkTree = Foldable.foldr insertTree emptyTree

  {-|
    'splits p partitions' takes a permutation 'p' and a list of integer
    partitions 'partitions' of some integers.
  -}
  splits :: [Int] -> [IntPartition.IntPartition] -> [[[Int]]]
  splits xs partitions = splitsAux xs t
    where
      t  = mkTree (fmap IntPartition.toList partitions)

  {-|
    Helper function for function 'splits'.
  -}
  splitsAux :: [Int] -> Tree -> [[[Int]]]
  splitsAux [] _ = [[]]
  splitsAux xs Tree { children=m } =
    [increasing:increasings | (k, t)      <- IntMap.toList m,
                              increasing  <- increasingsL xs k,
                              increasings <- splitsAux (xs L.\\ increasing) t]

  {-|
    'increasingsL xs k' returns the list of all increasing subsequences
    of length 'k' of the Int list 'xs'.
  -}
  increasingsL :: [Int] ->  Int -> [[Int]]
  increasingsL []       _ = []
  increasingsL xs@(x:_) k = increasingsLAux xs k k x

  {-|
    Helper function for function 'increasingsL'.
  -}
  increasingsLAux ::  [Int] ->  Int -> Int -> Int -> [[Int]]
  increasingsLAux _      _ 0 _  = [[]]
  increasingsLAux []     _ _ _  = []
  increasingsLAux (x:xs) k k' x'
    | k == k' || x > x' = fmap (x:) xss `Monoid.mappend` increasingsLAux xs k k' x'
    | otherwise         = increasingsLAux xs k k' x'
    where
      xss = increasingsLAux xs k (k'-1) x
