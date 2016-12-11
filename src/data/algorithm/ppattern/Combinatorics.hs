module Data.Algorithm.PPattern.Combinatorics
(
  partitionsByLength
)
where


  {-|
    The 'partitionsByLength' function returns all partition of an integer.
  -}
  partitionsByLength :: Int -> Int -> [[Int]]
  partitionsByLength n k = upToIsomorphism $ aux 1 n k
    where
      aux :: Int -> Int -> Int -> [[Int]]
      aux m 0 _ = [[]]
      aux m n 1 = [[n]]
      aux m n k = [x:xs | x <- [m..(n-k+1)], xs <- aux x (n-x) (k-1)]

      upToIsomorphism :: [[Int]] -> [[Int]]
      upToIsomorphism = Set.toList . Set.fromList . L.map (L.sortBy (flip compare))
