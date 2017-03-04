{-|
Module      : Data.Algorithm.PPattern
Structription : Short Structription
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer Structription of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern
(
  search
)
where

  import qualified Data.Foldable as Foldable

  import qualified Data.Algorithm.PPattern.Perm      as Perm
  import qualified Data.Algorithm.PPattern.CPoint    as CPoint
  import qualified Data.Algorithm.PPattern.Strategy  as Strategy
  import qualified Data.Algorithm.PPattern.State     as State

  type Context = (IntMap.IntMap Int, IntMap.IntMap Int)

  -- Make an initial list of colored points. Each element from the longest
  -- decreasing subsequence is given a distinct colors. All other elements
  -- are given the 'not determined yet' color 0.
  mkCPoints :: [(Int, Int)] -> [Int] -> [Color.Color] -> [Cpoint.Cpoint]
  mkCPoints []             _            _         = []
  mkCPoints ((i, x) : ixs) []           refColors = cp : mkCPoints ixs [] refColors
    where
      cp = CPoint.mkCPoint i x 0
  mkCPoints ((_, _) : _)   _            []        = error "We shouldn't be there"
  mkCPoints ((i, x) : ixs) ys'@(y : ys) refColors'@(c : refColors)
    | x == y    = CPoint.mkCPoint i x c : mkCPoints ixs ys  refColors
    | otherwise = CPoint.mkCPoint i x 0 : mkCPoints ixs ys' refColors'

  {-|
    The 'search' function takes two permutations 'p' and 'q', and it returns
    (if it exists) an order-isomorphic occurrence of 'p' in 'q'.
  -}
  search :: Perm.Perm -> Perm.Perm -> Strategy.Strategy -> Maybe State.Embedding
  search p q strategy
    | m > n     = Nothing
    | l > k     = Nothing
    | otherwise = res >>= State.embedding
    where
      -- compare by size
      m = Perm.size p
      n = Perm.size q

      -- comapre by longest decreasing subsequence length
      l = Perm.longestDecreasingLength p
      k = Perm.longestDecreasingLength q

      -- make initial state
      s = State.mkState q

      res = Foldable.asum [doSearch cps [1..k] (prevMap, nextMap) s
                          | refColors   <- [1..k] `Combi.choose` l
                          , let pcps    = mkCPoints pIndexed decreasing refColors
                          , let prevMap = IntMap.empty
                          , let nextMap = IntMap.fromList $ L.zip refColors decreasing]

  doSearch :: [CPoint.CPoint]   ->
              [Color.Color]     ->
              Context           ->
              Strategy.Strategy ->
              State.State       ->
              Maybe State.State
  doSearch []           _  _                  _        _  = Nothing
  doSearch (pcp : pcps) cs (prevMap, nextMap) strategy s
    | CPoint.color pcp == 0 = doSearchAux1 pcp pcps cs (prevMap, nextMap) strategy s
    | otherwise             = doSearchAux2 pcp pcps cs (prevMap, nextMap) strategy s

  -- pcp is a 0-color point.
  doSearchAux1 :: CPoint.CPoint     ->
                  [CPoint.CPoint]   ->
                  [Color.Color]     ->
                  Context           ->
                  Strategy.Strategy ->
                  State.State       ->
                  Maybe State.State
  doSearchAux1 pcp pcps cs (prevMap, nextMap) s =
      Foldable.asum [go strategy s' >>= doSearch pcps prevMap' nextMap' strategy
                    | c <- cs
                    , let x = CPoint.xCoord pcp
                    , let y = CPoint.yCoord pcp
                    , agreeWithPrevElement y c prevMap
                    , agreeWithNextElement y c nextMap
                    , let pcp'     = CPoint.mkCPoint x y c
                    , let prevMap' = updatePrevMap y c prevMap
                    , let nextMap' = updateNextMap y c nextMap
                    , let s'       = State.pAppend pcp'
                    ]

  -- pcp is not a 0-color point.
  doSearchAux2 :: CPoint.CPoint     ->
                  [CPoint.CPoint]   ->
                  [Color.Color]     ->
                  Context           ->
                  Strategy.Strategy ->
                  State.State       ->
                  Maybe State.State
  doSearchAux2 pcp pcps cs (prevMap, nextMap) strategy s =
    go strategy s' >>= doSearch pcps (prevMap', nextMap') strategy
    where
      prevMap' = updatePrevMap y c prevMap
      nextMap' = updateNextMap y c nextMap
      s'       = State.pAppend pcp

  agreeWithPrevElement :: Color.Color -> IntMap.Key -> IntMap.IntMap Int -> Bool
  agreeWithPrevElement x c m = case IntMap.lookup c m of
                                 Nothing -> True
                                 Just x' -> x' < x

  agreeWithNextElement :: Color.Color -> IntMap.Key -> IntMap.IntMap Int -> Bool
  agreeWithNextElement x c m = case IntMap.lookup c m of
                                 Nothing -> True
                                 Just x' -> x < x'

  updatePrevMap :: Color.Color       ->
                   IntMap.Key        ->
                   IntMap.IntMap Int ->
                   IntMap.IntMap Int
  updatePrevMap x= updatePrevMapAux x 1

  updatePrevMapAux :: Int               ->
                      IntMap.Key        ->
                      IntMap.Key        ->
                      IntMap.IntMap Int ->
                      IntMap.IntMap Int
  updatePrevMapAux x c c' m
    | c > c'    = m
    | otherwise = updatePrevMapAux  x (c+1) c' m'
      where
        m' = case IntMap.lookup c m of
               Nothing -> IntMap.insert c x m
               Just _  -> IntMap.update (Just . max x) c m

  updateNextMap :: Color.Color       ->
                   IntMap.Key        ->
                   IntMap.IntMap Int ->
                   IntMap.IntMap Int
  updateNextMap x c m =
    case IntMap.lookup c m of
      Nothing       -> m
      Just x'
        | x < x'    -> m
        | x == x'   -> IntMap.delete c m
        | otherwise -> error "We shouldn't be there" -- make ghc -Werror happy

  go :: Strategy.Strategy -> State.State -> Maybe State.State
  go strategy s =
    case strategy s of
      Nothing                    -> Just s
      Just (OrderConflict pcp t) -> State.xResolve pcp t s >>=
                                    go strategy
      Just (ValueConflict pcp t) -> State.yResolve pcp t s >>=
                                    go strategy
