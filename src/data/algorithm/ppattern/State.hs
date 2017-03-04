{-|
Module      : Data.Algorithm.PPattern.State
Structription : Short Structription
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer Structription of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.State
(
  -- * The @State@ type
  State
, empty

  -- * Constructing
, mkState

 -- * Querying
, qColors

  -- * The @Embeddingp@ type
, Embedding

  -- * The @Next@ type
, Next
)
where

  import qualified Data.List          as L
  import qualified Data.Set           as Set
  import qualified Data.Tuple         as T
  import qualified Data.Function      as Fun
  import qualified Data.Map.Strict    as Map
  import qualified Data.IntMap.Strict as IntMap
  import qualified Data.Monoid        as Monoid

  import qualified Data.Algorithm.PPattern.Perm      as Perm
  import qualified Data.Algorithm.PPattern.CPoint    as CPoint
  import qualified Data.Algorithm.PPattern.Color     as Color
  import qualified Data.Algorithm.PPattern.Embedding as Next
  import qualified Data.Algorithm.PPattern.Next      as Next

  -- colored point to colored points mapping
  type Embedding = Map.Map CPoint.CPoint CPoint.CPoint

  -- Fast access to colored point by color
  type Access = IntMap.IntMap CPoint.CPoint

  -- Next colored point mapping
  type Next = Map.Map CPoint.CPoint CPoint.CPoint

  -- The state of a search
  data State = State { pCPoints          :: [CPoint.CPoint]
                     , qCPoint           :: [CPoint.CPoint]
                     , embedding         :: Embedding
                     , pRightmostByColor :: Access
                     , qLeftmostByColor  :: Access
                     , qRightmostByColor :: Access
                     , qRightmost        :: Maybe CPoint.CPoint
                     , pNext             :: Next
                     , qNext             :: Next
                     } deriving (Eq)

  -- pretty print
  instance Show State where
    show State { embedding = e } = show e

  ------------------------------------------------------------------------------
  --
  -- Next
  --
  ------------------------------------------------------------------------------

  emptyNext :: Next
  emptyNext = IntMap.empty

  mkNext :: [CPoint.CPoint] -> Next
  mkNext = T.fst $ Foldable.foldr f (emptyNext, IntMap.empty)
    where
      f cp (m, m') = case IntMap.lookup c m' of
                       Nothing  -> (m, IntMap.insert c cp m')
                       Just cp' -> (insertNext cp cp' m, IntMap.update (\_ -> Just cp) c m')
        where
          c = CPoint.color cp

  lookupNext :: CPoint.CPoint -> Next -> Maybe CPoint.CPoint
  lookupNext = Map.lookup

  insertNext :: CPoint.CPoint -> CPoint.CPoint -> Next -> Next
  insertNext = Map.insert

  updateNext :: CPoint.CPoint -> CPoint.CPoint -> Next -> Next
  updateNext cp cp' n = case Map.lookup cp m of
                          Nothing -> Map.insert cp cp' m
                          Just _  -> Map.update (\_ -> Just cp') cp m

  jumpThreshold :: (CPoint.CPoint -> Int) -> Int -> Next -> CPoint.CPoint -> Maybe CPoint.CPoint
  jumpThreshold coordFun t n cp = aux (next cp n)
    where
      aux Nothing = Nothing
      aux (Just cp')
        | coordFun cp' > t = Just cp'
        | otherwise        = aux (next cp' n)

  xJumpThreshold :: Int -> Next -> CPoint.CPoint -> Maybe CPoint.CPoint
  xJumpThreshold = jumpThreshold CPoint.xCoord

  yJumpThreshold :: Int -> Next -> CPoint.CPoint -> Maybe CPoint.CPoint
  yJumpThreshold = jumpThreshold CPoint.yCoord

  ------------------------------------------------------------------------------
  --
  -- Embedding
  --
  ------------------------------------------------------------------------------

  emptyEmbedding :: Embedding
  emptyEmbedding  = Map.empty

  embeddingToList :: Embedding -> [(CPoint.CPoint, CPoint.CPoint)]
  embeddingToList = Map.toList

  insertEmbedding :: CPoint.CPoint -> CPoint.CPoint -> Embedding -> Embedding
  insertEmbedding = Map.insert

  lookupEmbedding :: CPoint.CPoint -> Embedding -> Maybe CPoint.CPoint
  lookupEmbedding = Map.lookup

  updateEmbedding :: CPoint.CPoint -> CPoint.CPoint -> Embedding -> Embedding
  updateEmbedding cp cp' m = case Map.lookup cp m of
                               Nothing -> Map.insert cp cp' m
                               Just _  -> Map.update (\ _ -> Just cp') cp m

  ------------------------------------------------------------------------------
  --
  -- Access
  --
  ------------------------------------------------------------------------------

  emptyAccess :: Access
  emptyAccess = IntMap.empty

  queryAccess :: Color.Color -> Access -> Maybe CPoint.CPoint
  queryAccess = IntMap.lookup

  mkLeftmostAccess :: [CPoint.CPoint] -> Access
  mkLeftmostAccess = Foldable.foldl f IntMap.empty
    where
      f m cp = case IntMap.lookup (CPoint.color cp) m of
                 Nothing -> IntMap.insert c cp m
                 Just _  -> m

  qQueryLeftmostBycolor :: Color.Color -> State -> Maybe CPoint.CPoint
  qQueryLeftmostBycolor c = queryAccess c . qLeftmostByColor

  mkRightmostAccess :: [CPoint.CPoint] -> Access
  mkRightmostAccess = Foldable.foldr f IntMap.empty
   where
     f cp m = case IntMap.lookup (CPoint.color cp) m of
                Nothing -> IntMap.insert c cp m
                Just _  -> m

  updateRightmostByColor :: CPoint.CPoint -> Access -> Access
  updateRightmostByColor cp m = m'
    where
      m  =
      m' = case IntMap.lookup (CPoint.color cp) m of
             Nothing -> IntMap.insert c cp m
             Just _  -> m

  pQueryRightmostAccess :: Color.Color -> State -> Maybe CPoint.CPoint
  pQueryRightmostAccess c = queryAccess c . pRightmost

  ------------------------------------------------------------------------------
  --
  -- State
  --
  ------------------------------------------------------------------------------

  {-|
    Make a new state. Permutation q is required.
  -}
  mkState :: Perm.Perm -> State
  mkState q  = State { pCPoints    = []
                     , qCPoint     = cps
                     , embedding   = emptyEmbedding
                     , pRightMost  = emptyAccess
                     , pRightMost' = Nothing
                     , qLeftMost   = mkLeftmostAccess qcps
                     , pNext       = emptyNext
                     , qNext       = n
                     }
    where
      qcps = mkQ q
      n    = mkNext qcps

  -- construct the colored points list associated to permutation q using
  -- a greedy coloring procedure.
  mkQ :: Perm.Perm -> [CPoint.CPoint]
  mkQ q = mkQAux [] (Perm.index q) IntMap.empty

  -- mkQ auxiliary function.
  mkQAux :: [CPoint.CPoint] -> [(Int, Int)] -> IntMap.IntMap Int -> [CPoint.CPoint]
  mkQAux acc []             _ = L.reverse acc
  mkQAux acc ((x, y) : xys) m = mkQAux (cp : acc) xys m'
    where
      c  = findSmallestColor y m
      m' = updateMap c y m
      cp = CPoint.mkCPoint x y c

  -- Auxialiary function for mkQAux.
  -- Find the smallest color for a new y-coordinate
  findSmallestColor :: Int -> IntMap.IntMap Int -> Int
  findSmallestColor y m = aux 1
    where
      aux c = case IntMap.lookup c m of
                Nothing -> c
                Just y' -> if y' < y then c else aux (c+1)

  -- Auxialiary function for mkQAux.
  -- Update the map for a given color
  updateMap :: Color.Color -> Int -> IntMap.IntMap Int -> IntMap.IntMap Int
  updateMap c y m = case IntMap.lookup c m of
                      Nothing -> IntMap.insert c y m
                      Just _  -> IntMap.update (\_ -> Just y) c m

  {-|
    The colors in permutation q.
  -}
  qColors :: State.State -> [Color.Color]
  qColors State { qCPoint = qcps } = Set.toList . Set.fromList . fmap Cpoint.color qcps

  {-|
    Add a new colored point to the list of colored points associated
    to permutation p.
  -}
  pAppend :: CPoint.CPoint -> State -> Maybe State
  pAppend pcp s = case lookupNext (CPoint.color pcp) (pRightmostByColor s) of
                    Nothing   -> pAppendAux1 pcp  pcp s
                    Just pcp' -> pAppendAux1 pcp' pcp s

  pAppendAux1 :: CPoint.CPoint -> CPoint.CPoint -> State -> Maybe State
  pAppendAux1 pcp pcp' s = case lookupEmbedding pcp s of
                             Nothing  -> lookupAccess (CPoint.color pcp) (qNext s) >>=
                                         pAppendAux2 pcp pcp'
                             Just qcp -> lookupNext (CPoint.color pcp) (qLeftmostByColor s) >>=
                                         pAppendAux2 pcp pcp'

  pAppendAux2 :: CPoint.CPoint -> CPoint.CPoint -> CPoint.CPoint -> State -> Maybe State
  pAppendAux2 pcp pcp' qcp s = case qRightmost s of
                                 Nothing   -> pAppendAux3 pcp pcp' qcp qcp
                                 Just qcp' -> pAppendAux3 pcp pcp' qcp' qcp

  pAppendAux3 :: CPoint.CPoint -> CPoint.CPoint -> CPoint.CPoint -> CPoint.CPoint -> State -> Maybe State
  pAppendAux3 pcp pcp' qcp qcp' s
    | x' < x    = lookupNext qcp' (qNext s) >>= pAppendAux3 pcp pcp' qcp
    | otherwise = pAppendFinalize pcp pcp' qcp qcp'
    where
      x  = CPoint.xCoord qcp
      x' = CPoint.xCoord qcp'

  pAppendFinalize :: CPoint.CPoint -> CPoint.CPoint -> CPoint.CPoint -> CPoint.CPoint -> State -> Maybe State
  pAppendFinalize pcp pcp' qcp qcp' s = Just s'
    where
      pCPoints'          = pCPoints s `Mondoid.mappend` [pcp']
      embedding'         = updateEmbedding pcp' qcp' (embedding s)
      pRightMostByColor' = updateAccess (CPoint.color pcp) pcp' (pRightMostByColor s)
      qRightmost         = qcp

      s'   = s { pCPoints          = pCPoints'
               , embedding         = embedding'
               , pRightMostByColor = pRightMostByColor'
               , qRightmost        = qRightmost'
               , pNext             = pNext'
               }

  xResolve :: CPoint.CPoint -> Int -> State -> Maybe State
  xResolve pcp t s = lookupEmbedding pcp (embedding s) >>=
                     xJumpThreshold t (qNext s)        >>=
                     resolve pcp s

  yResolve :: CPoint.CPoint -> Int -> State -> Maybe State
  yResolve pcp t s = lookupEmbedding pcp (embedding s) >>=
                     yJumpThreshold t (qNext s)       >>=
                     resolve pcp s

  resolve :: CPoint.CPoint -> State -> CPoint.CPoint -> Maybe State
  resolve pcp s qcp = resolveAux (next pcp (pNext s)) (next qcp (qNext s)) s
    where
      embedding' = updateEmbedding pcp qcp (embedding s)

      qRightMost' = case qRightmost s of
                      Nothing   -> Just qcp
                      Just qcp' -> if CPoint.xCoord qcp > CPoint.xCoord qcp'
                                   then Just qcp
                                   else Just qcp'

      qRightmostByColor' = case lookupNext (CPoint.color qcp) (qRightmostByColor s) of
                             Nothing   -> updateNext (CPoint.color qcp) qcp (qRightmostByColor s)
                             Just qcp' -> if CPoint.xCoord qcp > CPoint.xCoord qcp'
                                          then updateNext (CPoint.color qcp) qcp (qRightmostByColor s)
                                          else qRightmostByColor s

      s' = s { embedding          = embedding'
             , qRightmost         = qRightmost'
             , qRightmostByColor' = qRightmostByColor'
             }

  resolveAux :: Maybe CPoint.CPoint -> Maybe CPoint.CPoint -> State -> Maybe State
  resolveAux Nothing    _          s = Just s
  resolveAux _          Nothing    s = Nothing
  resolveAux (Just pcp) (Just qcp) s = imageEmbedding pcp (embedding s) >>=
                                       resolveAux' pcp qcp s

  resolveAux' :: CPoint.CPoint -> CPoint.CPoint -> State -> CPoint.CPoint -> Maybe State
  resolveAux' pcp qcp s qcp'
    | CPoint.xCoord qcp <= CPoint.xCoord qcp' = Just s'
    | otherwise                               = resolve pcp s qcp
    where
      qRightmostByColor' = a
      qRightmost' = b

      s' = s { qRightmostByColor = qRightmostByColor'
             , qRightmost = qRightmost'
             }
