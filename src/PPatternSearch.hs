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

{-# LANGUAGE DeriveDataTypeable #-}

import System.Console.CmdArgs
import System.Random
import Criterion.Main
import Data.Tuple.HT

import qualified Data.Algorithm.PPattern           as PPattern
import qualified Data.Algorithm.PPattern.Perm      as Perm
import qualified Data.Algorithm.PPattern.Strategy  as Strategy

data Options = Options { psize :: Int
                       , qsize :: Int
                       , nb    :: Int
                       , param :: Int
                       , seed  :: Int
                       } deriving (Data, Typeable)

options :: Options
options = Options { psize  = def &= help "The length of each source permutation"
                  , qsize = def &= help "The length of each target permutation"
                  , nb       = def &= help "The number of target permutations"
                  , param    = def &= help "The maximum number of increasing subsequence"
                  , seed     = def &= help "Random number generator seed"
                  }
                  &= verbosity
                  &= summary "ppattern-rand v0.1.0.0, (C) Stéphane Vialette 2017"
                  &= program "ppattern-rand"

go :: (RandomGen g) => Options -> g -> [Benchmark]
go opts = aux [] (nb opts)
  where
    pSize = psize opts
    qSize = qsize opts
    k     = param opts

    aux :: (RandomGen g) => [Benchmark] -> Int -> g -> [Benchmark]
    aux acc 0 _ = acc
    aux acc n g = aux (bench "search" (whnf f (p, q, Strategy.simple)) : acc) (n-1) g''
      where
        (p, g')  = Perm.randKIncreasing pSize k g
        (q, g'') = Perm.randKIncreasing qSize k g'
        f        = uncurry3 PPattern.search

main :: IO ()
main = do
  opts <- cmdArgs options
  defaultMain [bgroup "ppattern" (go opts (mkStdGen (seed opts)))]
