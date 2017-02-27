{-|
Module      : Data.Algorithm.PPattern.Random
Description : Short description
Copyright   : (c) Stéphane Vialette,2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module,containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Random
(
  randChoose
, randSelect
, randPerm
, randShuffle

  --
, sample
)
where

  import qualified Data.List      as L
  import qualified System.Random  as Random

  import qualified Data.Algorithm.PPattern.Tools as Tools
  import qualified Data.Algorithm.PPattern.Combi as Combi

  {-|
    'randChoose' takes a list 'xs', an integer 'k' and a generator 'g', and
    returns a random sublist of 'xs' of length 'k'), together with a new generatoRandom.
  -}
  randChoose :: Random.RandomGen g => [a] -> Int -> g -> ([a], g)
  randChoose xs k g = (xss L.!! (i-1), g')
    where
      xss = xs `Combi.choose` k
      (i, g') = Random.randomR (1, L.length xss) g

  {-|
    'randSelect' takes a list 'xs', an integer 'k' and a generator 'g', and
    returns a random list 'xs' of length 'k' of random selected elements from
    'xs', together with a new generatoRandom.
  -}
  randSelect :: Random.RandomGen g => [a] -> Int -> g -> ([a], g)
  randSelect xs = randSelectAux xs []

  randSelectAux :: Random.RandomGen g => [a] -> [a] -> Int -> g -> ([a], g)
  randSelectAux xs acc k g
    | k == 0    = (acc, g)
    | otherwise = randSelectAux (Tools.removeAt' xs i) (x:acc) (k-1) g'
    where
      n       = L.length xs
      (i, g') = Random.randomR (0, n-1) g
      x       = xs L.!! i

  {-|
    'randPerm' takes a list 'xs' and a generator 'g', and
    returns a random permutation of 'xs', together with a new generatoRandom.
  -}
  randPerm :: Random.RandomGen g => [a] -> g -> ([a], g)
  randPerm xs = randSelect xs (L.length xs)

  {-|
    'randShuffle' takes a list of lists 'xss' and a generator 'g', and
    returns a random shuffle of xss (i.e. each list of 'xss' is a sublist of the
    result), together with a new generatoRandom.
  -}
  randShuffle :: Random.RandomGen g => [[a]] -> g -> ([a], g)
  randShuffle xss = randShuffleAux xss []

  randShuffleAux :: Random.RandomGen g => [[a]] -> [a] -> g -> ([a], g)
  randShuffleAux []  acc g = (L.reverse acc, g)
  randShuffleAux xss acc g = randShuffleAux xss'' (x:acc) g'
    where
      (xss', g') = randPerm xss g
      (x, xss'') = randShuffleAux' xss'

  randShuffleAux' :: [[a]] -> (a, [[a]])
  randShuffleAux' []            = error "We shouldn't be there"
  randShuffleAux' ([]:_)        = error "We shouldn't be there"
  randShuffleAux' ([x]:xss)     = (x, xss)
  randShuffleAux' ((x:xs):xss') = (x, xs:xss')

  pick :: (Random.RandomGen g, Eq a) => [a] -> g -> (a, g)
  pick xs g = (xs !! i, g')
    where
      (i, g') = Random.randomR (0, length xs - 1) g

  sample :: (Random.RandomGen g, Eq a, Ord a) => Int -> [a] -> g -> ([a], g)
  sample n xs g = (L.sort ys, g')
    where
      (ys, g') = sampleAux n xs g

  sampleAux :: (Random.RandomGen g, Eq a, Ord a) => Int -> [a] -> g -> ([a], g)
  sampleAux 0 _  g = ([], g)
  sampleAux n xs g = (x : ys, g'')
    where
      (x, g')   = pick xs g
      (ys, g'') = sampleAux (n-1) (L.delete x xs) g'
