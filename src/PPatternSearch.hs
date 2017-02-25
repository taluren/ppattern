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
import Data.Maybe
import qualified Data.List as L

import qualified Data.Algorithm.PPattern           as PPattern
import qualified Data.Algorithm.PPattern.Perm      as Perm
import qualified Data.Algorithm.PPattern.Strategy  as Strategy
import qualified Data.Algorithm.PPattern.Embedding as Embedding

data Options = Options { srcSize  :: Int
                       , srcNb    :: Int
                       , trgtSize :: Int
                       , trgtNb   :: Int
                       , param    :: Int
                       , seed     :: Int
                       } deriving (Data, Typeable)

options :: Options
options = Options { srcSize  = def &= help "The length of each source permutation"
                  , srcNb    = def &= help "The number of sources permutations"
                  , trgtSize = def &= help "The length of each target permutation"
                  , trgtNb   = def &= help "The number of target permutations"
                  , param    = def &= help "The maximum number of increasing subsequence"
                  , seed     = def &= help "Random number generator seed"
                  }
                  &= verbosity
                  &= summary "ppattern-rand v0.1.0.0, (C) Stéphane Vialette 2017"
                  &= program "ppattern-rand"

permutations :: RandomGen g => Int -> Int -> Int -> g -> ([Perm.Perm], g)
permutations n k' m = aux [] 1
  where
    aux acc i g
      | i == m    = (acc, g)
      | otherwise = aux (p : acc) (i+1) g'
        where
          (p, g') = Perm.randKIncreasing n k' g

go :: RandomGen g => Options -> g -> ([Maybe Embedding.Embedding], g)
go opts g = (res, g'')
  where
    (sourcePermutations, g')  = permutations (srcSize opts) (param opts) (srcNb opts) g
    (targetPermutations, g'') = permutations (trgtSize opts) (param opts) (trgtNb opts) g'
    s                         = Strategy.simple
    res                       = [PPattern.search p q s | p <- sourcePermutations
                                                       , q <- targetPermutations]

main :: IO ()
main = do
  opts <- cmdArgs options
  print . L.length . L.filter isJust . fst $ go opts (mkStdGen (seed opts))
