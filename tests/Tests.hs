module Main
  where

    import Test.Framework (defaultMain)

    import Data.Algorithm.PPattern.IntPartition.Test

    main :: IO ()
    main = defaultMain [intPartitionSuite]
