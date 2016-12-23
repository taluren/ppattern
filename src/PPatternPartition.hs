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

import qualified Data.Algorithm.PPattern.Permutation as Permutation

data Options = Options { inputFilename  :: FilePath
                       , outputFilename :: FilePath
                       } deriving (Data, Typeable)

options :: Options
options = Options { inputFilename  = def &= help "input filename"
                  , outputFilename = def &= help "output filename"
                  }
                  &= verbosity
                  &= summary "ppattern-partition v0.1.0.0, (C) Stéphane Vialette 2016"
                  &= program "ppattern-partition"

go :: String -> [(Permutation.Permutation, [Int], Int, [Int], Int)]
go = foldr f [] . fmap (\p -> read p :: Permutation.Permutation) . lines
  where
    f p acc = (p, l1s, l1, l2s, l2):acc
      where
        l1s = fmap Permutation.size (Permutation.greedyPartitionIncreasings1 p)
        l1  = length l1s
        l2s = fmap Permutation.size (Permutation.greedyPartitionIncreasings2 p)
        l2  = length l2s

main :: IO ()
main = do
  opts     <- cmdArgs options
  contents <- readFile (inputFilename opts)
  writeFile (outputFilename opts) . unlines . fmap show $ go contents
  -- mapM_ print $ go contents
