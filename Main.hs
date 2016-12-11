import PPattern

main :: IO ()
main = putStrLn $ show $ partitionsInIncreasings [5,2,3,7,1,4,6] 3
