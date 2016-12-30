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

import qualified Data.Algorithm.PPattern.Perm as Perm
import qualified Data.Algorithm.PPattern.CPoint as CPoint
-- import qualified Data.Algorithm.PPattern.Strategy as Strategy
import Data.Algorithm.PPattern

main :: IO ()
-- main = putStrLn $ show $ partitionsIncreasings (Seq.fromList [1..8]) 3
main =
  let p = Perm.fromIntList [1,2,3]
      q = Perm.fromIntList [4,5,6,1,2,3]
      (cpsQ, intPartitionQ) = mkQ q

  in do
    print $ mkPs p (CPoint.nbColors cpsQ) intPartitionQ
    -- print $ search p q Strategy.simple
