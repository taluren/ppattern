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

-- )
where

  import qualified Data.List      as L
  import qualified Data.Foldable      as Foldable
  import qualified Data.IntMap.Strict as IntMap
  import qualified Data.Monoid        as Monoid

  import qualified Data.Algorithm.PPattern.Perm as Perm
  import qualified Data.Algorithm.PPattern.IntPartition as IntPartition
  import qualified Data.Algorithm.PPattern.Types as T

  newtype Tree = Tree { children :: IntMap.IntMap Tree }
                 deriving (Show)

  emptyTree :: Tree
  emptyTree = Tree { children=IntMap.empty }

  lookupTree :: Int -> Tree -> Maybe Tree
  lookupTree x Tree { children=m } = IntMap.lookup x m

  insertTree :: [Int] -> Tree -> Tree
  insertTree []     t                   = t
  insertTree (x:xs) Tree { children=m } = case IntMap.lookup x m of
    Nothing -> Tree { children=IntMap.insert x (path xs)                      m }
    Just t  -> Tree { children=IntMap.update (\_ -> Just (insertTree xs t)) x m }

  path :: [Int] -> Tree
  path = Foldable.foldr (\x t -> Tree { children=IntMap.singleton x t }) emptyTree

  mkTree :: [[Int]] -> Tree
  mkTree = Foldable.foldr insertTree emptyTree

  {-|
    'splits p partitions' takes a permutation 'p' and a list of integer
    partitions 'partitions' of some integers.
  -}
  splits :: Perm.Perm -> [IntPartition.IntPartition] -> [[Perm.Perm]]
  splits p partitions = fmap Perm.fromListUnsafe <$> splitAux xs t
    where
      xs = Perm.toList p
      t  = mkTree (fmap IntPartition.toList partitions)

  {-|
    Helper function for function 'splits'.
  -}
  splitsAux :: [T.T] -> Tree -> [[[T.T]]]
  splitsAux [] _ = [[]]
  splitsAux xs Tree { children=m } =
    [increasing:increasings | (k, t)      <- IntMap.toList m,
                              increasing  <- increasingsL xs k,
                              increasings <- splitsAux (xs L.\\ increasing) t]

  {-|
    'increasingsL' xs k' returns the list of all increasing subsequences
    of length 'k' of the T.T list 'xs'.
  -}
  increasingsL :: [T.T] ->  Int -> [[T.T]]
  increasingsL []       _ = []
  increasingsL xs@(x:_) k = increasingsLAux xs k k x

  {-|
    Helper function for function 'increasingsL'.
  -}
  increasingsLAux ::  [T.T] ->  Int -> Int -> T.T -> [[T.T]]
  increasingsLAux _      _ 0 _  = [[]]
  increasingsLAux []     _ _ _  = []
  increasingsLAux (x:xs) k k' x'
    | k == k' || x > x' = fmap (x:) xss `Monoid.mappend` increasingsLAux xs k k' x'
    | otherwise         = increasingsLAux xs k k' x'
    where
      xss = increasingsLAux xs k (k'-1) x
