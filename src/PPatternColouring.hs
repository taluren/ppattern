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

{-# OPTIONS_GHC -fno-cse #-}

import Data.List
-- import Criterion.Main
import Data.Maybe

import qualified Data.Algorithm.PPattern.Perm     as Perm
import qualified Data.Algorithm.PPattern.Strategy as Strategy
-- import qualified Data.Algorithm.PPattern.Combi    as Combi
import Data.Algorithm.PPattern

main :: IO ()
main = print $ length [isJust(search (Perm.fromIntList xs) q Strategy.simple) | xs <- sub]
  where
    l = [22,7,19,28,5,14,2,23,13,1,24,21,27,11,6,4,16,15,26,8,9,25,20,17,30,10,18,3,29,12]
    -- sub = l `Combi.choose` 3
    sub = permutations [1..10]
    q = Perm.fromIntList l

  -- main :: IO ()
  -- main = defaultMain [
  --   bgroup "ppattern"
  --   [bench (show xs) $ whnf isJust(search (Perm.fromIntList xs) q Strategy.simple) | xs <- sub]
  --   ]
  --     where
  --       l = [22,7,19,28,5,14,2,23,13,1,24,21,27,11,6,4,16,15,26,8,9,25,20,17,30,10,18,3,29,12]
  --       -- sub = l `Combi.choose` 3
  --       sub = permutations [1..4]
  --       q = Perm.fromIntList l
