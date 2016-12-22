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
{-# OPTIONS_GHC -fno-cse #-}

import Data.Algorithm.PPattern.Permutation

main :: IO ()
-- main = putStrLn $ show $ partitionsIncreasings (Seq.fromList [1..8]) 3
main = print $ Permutation [4,5,3,6,1,2]
