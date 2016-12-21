import Data.Algorithm.PPattern.Permutation
import Data.Algorithm.PPattern


main :: IO ()
-- main = putStrLn $ show $ partitionsIncreasings (Seq.fromList [1..8]) 3
main = print $ search (Permutation [1,2,3]) (Permutation [4,5,3,6,1,2])
