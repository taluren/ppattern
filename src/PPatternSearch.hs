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
import Data.Monoid as Monoid

import qualified Data.Algorithm.PPattern.Strategy as Strategy
import qualified Data.Algorithm.PPattern          as PPattern
import qualified Data.Algorithm.PPattern.Perm     as Perm

data Options = Options { pattern :: String
                       , target  :: String
                       } deriving (Data, Typeable)

options :: Options
options = Options { pattern = def &= help "The pattern permutation"
                  , target  = def &= help "The target permutation"
                  }
                  &= verbosity
                  &= summary "ppattern-search v0.1.0.0, (C) Stéphane Vialette 2017"
                  &= program "ppattern-search"

main :: IO ()
main = do
  opts <- cmdArgs options
  putStrLn $ PPattern.search (read $ pattern opts) (read $ target  opts) Strategy.anyConflict
