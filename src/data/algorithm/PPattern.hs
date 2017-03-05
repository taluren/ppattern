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
where

  import qualified Data.List          as L
  import qualified Data.IntMap.Strict as IntMap
  import qualified Data.Foldable      as Foldable

  import qualified Data.Algorithm.PPattern.Perm      as Perm
  import qualified Data.Algorithm.PPattern.CPoint    as CPoint
  import qualified Data.Algorithm.PPattern.Strategy  as Strategy
  import qualified Data.Algorithm.PPattern.Color     as Color
  import qualified Data.Algorithm.PPattern.State     as State
  import qualified Data.Algorithm.PPattern.Conflict  as Conflict
  import qualified Data.Algorithm.PPattern.Combi     as Combi

  -- Context type for constructing increasing colorings of permutation p.
  data Context = Context { precede :: IntMap.IntMap Int
                         , follow  :: IntMap.IntMap Int
                         } deriving (Eq, Show)

  mkContext :: IntMap.IntMap Int -> IntMap.IntMap Int -> Context
  mkContext m m' = Context { precede = m, follow = m' }

  -- Make an initial list of colored points. Each element from the longest
  -- decreasing subsequence is given a distinct colors. All other elements
  -- are given the 'not determined yet' color 0.
  mkCPoints :: [(Int, Int)] -> [Int] -> [Color.Color] -> [CPoint.CPoint]
  mkCPoints []             _  _         = []
  mkCPoints ((i, x) : ixs) [] refColors = cp : mkCPoints ixs [] refColors
    where
      cp = CPoint.mkCPoint i x 0
  mkCPoints ((_, _) : _)   _            []        =
    error "mkCPoints. We shouldn't be there" -- make ghc -Werror happy
  mkCPoints ((i, x) : ixs) ys'@(y : ys) refColors'@(c : refColors)
    | x == y    = CPoint.mkCPoint i x c : mkCPoints ixs ys  refColors
    | otherwise = CPoint.mkCPoint i x 0 : mkCPoints ixs ys' refColors'

  {-|
    The 'search' function takes two permutations 'p' and 'q', and it returns
    (if it exists) an order-isomorphic occurrence of 'p' in 'q'.
  -}
  search :: Perm.Perm -> Perm.Perm -> Strategy.Strategy -> Maybe State.Embedding
  search p q strategy = searchAux p q strategy >>= (Just . State.embedding)

  searchAux :: Perm.Perm -> Perm.Perm -> Strategy.Strategy -> Maybe State.State
  searchAux p q strategy
    | m > n     = Nothing
    | l > k     = Nothing
    | otherwise = res
    where
      -- compare by size
      m = Perm.size p
      n = Perm.size q

      -- comapre by longest decreasing subsequence length
      decreasing = Perm.longestDecreasing p
      l = Foldable.length decreasing
      k = Perm.longestDecreasingLength q

      -- make initial state
      s = State.mkState q
      cs = [1..k]
      pIndexed = Perm.index p

      -- Perform search
      res = Foldable.asum [doSearch pcps cs context strategy s
                            | refColors   <- cs `Combi.choose` l
                            , let pcps    = mkCPoints pIndexed decreasing refColors
                            , let precedeMap = IntMap.empty
                            , let followMap = IntMap.fromList $ L.zip refColors decreasing
                            , let context = mkContext precedeMap followMap
                          ]

  doSearch ::
    [CPoint.CPoint] -> [Color.Color] -> Context -> Strategy.Strategy -> State.State ->
      Maybe State.State
  doSearch []             _  _       _        s  = Just s
  doSearch pcps@(pcp : _) cs context strategy s
    | CPoint.color pcp == 0 = doSearchAux1 pcps cs context strategy s
    | otherwise             = doSearchAux2 pcps cs context strategy s

  -- pcp is a 0-color point.
  doSearchAux1 ::
    [CPoint.CPoint] -> [Color.Color] -> Context -> Strategy.Strategy -> State.State ->
      Maybe State.State
  doSearchAux1 []           _  _       _        _ =
    error "doSearchAux1. We shouldn't be there" -- make ghc -Werror happy
  doSearchAux1 (pcp : pcps) cs context strategy s =
    Foldable.asum [State.pAppend (CPoint.mkCPoint x y c) s >>= -- append new point
                   go strategy                             >>= -- resolve for match
                   doSearch pcps cs context' strategy
                    | c <- cs
                    , let precedeMap = precede context
                    , let followMap  = follow context
                    , agreeWithprecedeMap y c precedeMap
                    , agreeWithfollowMap y c followMap
                    , let precedeMap' = updatePrecedeMap y c precedeMap
                    , let followMap'  = updateFollowMap y c followMap
                    , let context'    = mkContext precedeMap' followMap'
                  ]
    where
      x = CPoint.xCoord pcp
      y = CPoint.yCoord pcp

  -- pcp is not a 0-color point.
  doSearchAux2 ::
    [CPoint.CPoint] -> [Color.Color] -> Context -> Strategy.Strategy -> State.State ->
      Maybe State.State
  doSearchAux2 []           _  _                  _        _ =
    error "doSearchAux2. We shouldn't be there" -- make ghc -Werror happy
  doSearchAux2 (pcp : pcps) cs context strategy s =
    State.pAppend pcp s >>= -- append new point
    go strategy         >>= -- resolve for match
    doSearch pcps cs context' strategy
    where
      y = CPoint.yCoord pcp
      c = CPoint.color pcp
      precedeMap' = updatePrecedeMap y c (precede context)
      followMap' = updateFollowMap y c (follow context)
      context' = mkContext precedeMap' followMap'

  agreeWithprecedeMap :: Color.Color -> IntMap.Key -> IntMap.IntMap Int -> Bool
  agreeWithprecedeMap y c m =
    case IntMap.lookup c m of
      Nothing -> True
      Just y' -> y' < y

  agreeWithfollowMap :: Color.Color -> IntMap.Key -> IntMap.IntMap Int -> Bool
  agreeWithfollowMap y c m =
    case IntMap.lookup c m of
      Nothing -> True
      Just y' -> y < y'

  updatePrecedeMap ::
    Color.Color -> IntMap.Key -> IntMap.IntMap Int -> IntMap.IntMap Int
  updatePrecedeMap y = updatePrecedeMapAux y 1

  updatePrecedeMapAux ::
    Int -> IntMap.Key -> IntMap.Key -> IntMap.IntMap Int -> IntMap.IntMap Int
  updatePrecedeMapAux y c c' m
    | c > c'    = m
    | otherwise = updatePrecedeMapAux y (c+1) c' m'
      where
        m' = case IntMap.lookup c m of
               Nothing  -> IntMap.insert c y m
               Just y'  -> IntMap.insert (max y y') c m

  updateFollowMap ::
    Color.Color -> IntMap.Key -> IntMap.IntMap Int -> IntMap.IntMap Int
  updateFollowMap y c m =
    case IntMap.lookup c m of
      Nothing       -> m
      Just y'
        | y < y'    -> m
        | y == y'   -> IntMap.delete c m
        | otherwise -> error "updateFollowMap. We shouldn't be there" -- make ghc -Werror happy

  go :: Strategy.Strategy -> State.State -> Maybe State.State
  go strategy s =
    case strategy s of
      Nothing                             -> Just s
      Just (Conflict.OrderConflict pcp t) -> State.xResolve pcp t s >>=
                                             go strategy
      Just (Conflict.ValueConflict pcp t) -> State.yResolve pcp t s >>=
                                             go strategy
