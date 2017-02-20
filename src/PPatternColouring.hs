{-|
Module      : PPatternColouring
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

{-# OPTIONS_GHC -fno-cse #-}

import Criterion.Main

import qualified Data.Algorithm.PPattern.Perm as Perm
-- import qualified Data.Algorithm.PPattern.Combi    as Combi

main :: IO ()
main = defaultMain [
  bgroup "ppattern"
    [ bench "Perm.greedyIncreasing1" $ whnf Perm.greedyIncreasing1 p
    , bench "Perm.greedyIncreasing2" $ whnf Perm.greedyIncreasing2 p
    ]
  ]
    where
      p = Perm.fromIntList [22,7,19,28,5,14,2,23,13,1,24,21,27,11,6,4,16,15,26,8,9,25,20,17,30,10,18,3,29,12]
