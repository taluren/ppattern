{-|
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

import qualified Data.Algorithm.PPattern.Permutation as Permutation

data Options = Options { len :: Int
                       , num :: Int
                       } deriving (Data, Typeable)

options :: Options
options = Options { len = 100  &= help "The length of each permutation"
                  , num = 1000 &= help "The number of generated permutations"
                  }
                  &= summary "ppattern-rand v0.1.0.0, (C) Stéphane Vialette 2016"
                  &= program "ppattern-rand"

go :: RandomGen g => Options -> g -> [Permutation.Permutation]
go opts = aux (len opts) (num opts) []
  where
    aux _ 0 acc _ = acc
    aux n m acc g = aux n (m-1) (p:acc) g'
      where
        (p, g') = Permutation.randPermutation' n g

main :: IO ()
main = do
  opts <- cmdArgs options
  g    <- getStdGen
  mapM_ print $ go opts g
