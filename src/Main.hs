import Data.Algorithm.PPattern.Splitting

main :: IO ()
main = putStrLn $ show $ partitionsInIncreasings [1..12] 3
