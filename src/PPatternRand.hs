{-|
Module      : PPatternRand
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

import System.Random
import Criterion.Main
import Data.Monoid as Monoid

import qualified Data.Algorithm.PPattern           as PPattern
import qualified Data.Algorithm.PPattern.Perm      as Perm
import qualified Data.Algorithm.PPattern.Strategy  as Strategy
import qualified Data.Algorithm.PPattern.State     as State

go :: (Perm.Perm, Perm.Perm) -> Maybe State.Embedding
go (p, q) = PPattern.search p q Strategy.anyConflict

mkPQs :: (RandomGen g) => g -> [Benchmark]
mkPQs = aux [] (1 :: Int)
  where
    l = 100
    m = 40
    n = 1000
    k = 4
    aux acc i g
      | i == l    = acc
      | otherwise = aux (pq : acc) (i+1) g''
      where
        (p, g')  = Perm.randKIncreasing m k g
        (q, g'') = Perm.randKIncreasing n k g'
        label    = "search (" `Monoid.mappend`
                   "p="       `Monoid.mappend`
                   show p     `Monoid.mappend`
                   ", q="     `Monoid.mappend`
                   show q     `Monoid.mappend`
                   ", k="     `Monoid.mappend`
                   show k     `Monoid.mappend`
                   ", i="     `Monoid.mappend`
                   show i     `Monoid.mappend`
                   ")"
        pq = bench label $ whnf go (p, q)

main :: IO ()
main =
  defaultMain [bgroup "ppattern" (mkPQs (mkStdGen 1423908765))]
