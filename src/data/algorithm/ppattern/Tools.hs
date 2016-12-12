sortPairFst :: Ord a => [(a, b)] -> [(a, b)]
sortPairFst = L.sortBy comp
  where
    comp t t' = (T.fst t1) `compare` (T.fst t2)

sortPairSnd :: Ord b => [(a, b)] -> [(a, b)]
sortPairSnd = L.sortBy comp
  where
    comp t t' = (T.snd t1) `compare` (T.snd t2)
