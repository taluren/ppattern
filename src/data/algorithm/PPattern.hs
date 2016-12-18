{-|
Module      : Data.Algorithm.PPattern
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern
(
  search
, resolve1
)
where

  import qualified Data.List         as L
  import qualified Data.Map.Strict   as Map
  import qualified Data.Tuple.HT     as THT
  import qualified Data.Array        as Array
  import qualified Data.Function     as Fun

  import qualified Data.Algorithm.PPattern.Point         as Point
  import qualified Data.Algorithm.PPattern.CPoint        as CPoint
  import qualified Data.Algorithm.PPattern.CPointLink    as CPointLink
  import qualified Data.Algorithm.PPattern.CPointMapping as CPointMapping


  {-|
    The 'makeCPointArray' function takes a list of colored points and return
    an array containing this point sorted by ascending x-coordinates.
  -}
  makeCPointArray :: [CPoint.CPoint c] -> Array.Array Int Int
  makeCPointArray cps = Array.listArray (1, n) cps'
    where
      n    = L.length cps
      cps' = L.sortBy (compare `Fun.on` (Point.xCoord . CPoint.point)) cps

  {-|
    'leftmostCPointMapping p q' return the leftmost color friendly mapping from 'p'
    to 'a'.
  -}
  leftmostCPointMapping :: [CPoint.Cpoint c] -> [CPoint.Cpoint c] -> Maybe [CPointLink c]
  leftmostCPointMapping [] []  = Just []
  leftmostCPointMapping [] _   = Just []
  leftmostCPointMapping _  []  = Nothing
  leftmostCPointMapping (cp1:cp1s) (cp2:cp2s)
    | c1 = c2   = makeCPointLink cp1 cp2 >>= \cpl -> fmap (cpl:) (aux cp1s cp2s)
    | otherwise = leftmostCPointMapping (cp1:cp1s) cp2s
    where
      c1 = CPoint.color cp1
      c2 = CPoint.color cp2

  {-|
    The 'makeTarget' function takes a list of integer, and it return a
    list of CPoints by mean of a greedy coloring.
  -}
  makeTargetCPointList :: Permutation -> [CPoint]
  makeTargetCPointList (Permutation xs) = makeCPoints xs m
    where
      iss = greedyPartitionIncreasings1 xs
      m = mapFromPartition iss

  makeSourceCPointLists :: [Int] -> Int -> [[CPoint]]
  makeSources ys k = aux (Splitting.partitionsIncreasings k)
    where
      aux []       = []
      aux (is:iss) = makeSource ys is:aux iss

  makeSource :: [Int] -> [[Int]] -> [[CPoint]]
  makeSource ys iss = makeCPoints ys m
    where
      m = mapFromPartition iss

  {-|
    The 'makeCPoints' function takes a list of integer and color mapping given
    in the form of a Map.Map, and it return a list of CPoint.
  -}
  makeCPoints :: [Int] -> Map.Map Int a -> [CPoint]
  makeCPoints xs m = L.map (THT.uncurry3 CPoint.makeCPoint') [] ts
    where
      is = [1..]
      cs = F.foldr (\x -> fromJust (Map.lookup x m)) xs
      ts = L.zip3 is xs cs

  {-|
    The 'square' function squares an integer.
    It takes one argument, of type 'Int'.
  -}
  search :: [Int] -> [Int] -> Maybe [Int]
  search xs ys = searchAux cxs cyss
    where
      cxs  = color xs
      k    = nbColors xs'
      cyss = colors ys k

  {-|
    The 'square' function squares an integer.
    It takes one argument, of type 'Int'.
  -}
  searchAux :: [CPoint Int Int] -> [[CPoint Int Int]]
  searchAux _   [] = Nothing
  searchAux cxs (cys:cyss) = case doSearch cxs cys of
    Nothing -> searchAux cxs cyss
    Just xs -> Just xs

  doSearch :: [CPoint Int Int] -> [[CPoint Int Int]] -> Maybe [Int]
  doSearch cxs cys = Nothing

  search p q = leftmostCPointMapping p q >>= aux
    where
      aux cm = aux' (resolve1 p q cm >>= resolve2 p q)
        where
          aux' Nothing  = Nothing
          aux (Just cm')
            | cm /= cm' = aux cm' -- keep on resolving conflicts
            | otherwise = cm'     -- conflict-free mapping, we're done


  {-|
    The 'square' function squares an integer.
    It takes one argument, of type 'Int'.
  -}
  -- resolve1 :: CPointMapping a c -> Maybe (CPointMapping a c)
  -- resolve1 []        = Just []
  -- resolve1 [l]       = Just [l]
  -- resolve1 (l:l':ls)
  --   | conflict l l' = fmap (l:) (resolve1 (update1 l':ls))
  --   | otherwise     = fmap (l:) (resolve1 l':ls)
  --   where
  --     bichromatic l l'   = colour l /= colour l'
  --     orderConflict l l' = (sPoint l) `southEastDomination` (tPoint l')
  --     confict l l'       = bichromatic l l' && orderConflict l l'
