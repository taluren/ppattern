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

-- {-# LANGUAGE DeriveDataTypeable #-}

-- import System.Console.CmdArgs
import System.Random
import Criterion.Main
import Data.Monoid as Monoid

import qualified Data.Algorithm.PPattern           as PPattern
import qualified Data.Algorithm.PPattern.Perm      as Perm
import qualified Data.Algorithm.PPattern.Strategy  as Strategy
import qualified Data.Algorithm.PPattern.State     as State

-- data Options = Options { psize :: Int
--                        , qsize :: Int
--                        , param :: Int
--                        , seed  :: Int
--                        } deriving (Data, Typeable)
--
-- options :: Options
-- options = Options { psize = def &= help "The length of each source permutation"
--                   , qsize = def &= help "The length of each target permutation"
--                   , param = def &= help "The maximum number of increasing subsequence"
--                   , seed  = def &= help "Random number generator seed"
--                   }
--                   &= verbosity
--                   &= summary "ppattern-search v0.1.0.0, (C) Stéphane Vialette 2017"
--                   &= program "ppattern-search"

-- go :: (RandomGen g) => Options -> g -> [Benchmark]
-- go opts = aux [] (nb opts)
--   where
--     pSize = psize opts
--     qSize = qsize opts
--     k     = param opts
--
--     aux :: (RandomGen g) => [Benchmark] -> Int -> g -> [Benchmark]
--     aux acc 0 _ = acc
--     aux acc n g = aux (bench "search" (whnf f (p, q, Strategy.simple)) : acc) (n-1) g''
--       where
--         (p, g')  = Perm.randKIncreasing pSize k g
--         (q, g'') = Perm.randKIncreasing qSize k g'
--         f        = uncurry3 PPattern.search
--
-- main :: IO ()
-- main = do
--   opts <- cmdArgs options
--   defaultMain [bgroup "ppattern" (go opts (mkStdGen (seed opts)))]

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
