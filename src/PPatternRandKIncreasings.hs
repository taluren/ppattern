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

import qualified Data.Algorithm.PPattern.Perm as Perm

data Options = Options { len            :: Int
                       , num            :: Int
                       , increasings    :: Int
                       , outputFilename :: FilePath
                       } deriving (Data, Typeable)

options :: Options
options = Options { len            = def &= help "The length of each permutation"
                  , num            = def &= help "The number of generated permutation"
                  , increasings    = def &= help "the number of increasing patterns the permutation can decomposed on"
                  , outputFilename = def &= help "The output filename"
                  }
                  &= verbosity
                  &= summary "ppattern-randk v0.1.0.0, (C) Stéphane Vialette 2016"
                  &= program "ppattern-randk"

go :: RandomGen g => Options -> g -> [Perm.Perm]
go opts = aux (len opts) (num opts) (increasings opts) []
  where
    aux _ 0 _ acc _ = acc
    aux n m k acc g = aux n (m-1) k (p:acc) g'
      where
        (p, g') = Perm.randKIncreasing n k g

main :: IO ()
main = do
  opts <- cmdArgs options
  g    <- getStdGen
  writeFile (outputFilename opts) . unlines . fmap show $ go opts g
