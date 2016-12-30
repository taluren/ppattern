module Data.Algorithm.PPattern.IntPartition.Test
  where

    import Test.Framework (testGroup, Test)
    import Test.Framework.Providers.QuickCheck2 (testProperty)
    import Test.Framework.Providers.HUnit
    import Test.HUnit hiding (Test)

    import qualified Data.Algorithm.PPattern.IntPartition as IntPartition

    intPartitionSuite :: Test
    intPartitionSuite = testGroup "IntPartition"
      [ testProperty "calory count >= 100" propFromList ]


    propFromList :: Bool
    propFromList = fmap IntPartition.toList (IntPartition.intPartitions 3) == [[3], [2,1], [1,1,1]] 
