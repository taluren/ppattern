{-|
Module      : Data.Algorithm.PPattern.Next
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

  -- * The @Embedding@ type
, Embedding

  -- * The @ColorCPointMap@ type
, ColorCPointMap

  -- * The @Next@ type
, Next
)
where

  import qualified Data.List          as L
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
  data State = State { pCPoints    :: [CPoint.CPoint]
                     , qCPoint     :: [CPoint.CPoint]
                     , embedding   :: Embedding
                     , pRightmost  :: Access
                     , pRightmost' :: Maybe CPoint.CPoint
                     , qLeftmost   :: Access
                     , pNext       :: Next
                     , qNext       :: Next
                     } deriving (Eq)

  -- pretty print
  instance Show State where
    show State { embedding = e } = show $ embeddingToList e

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

  next :: CPoint.CPoint -> Next -> Maybe CPoint.CPoint
  next = lookupNext

  next' :: Next -> CPoint.CPoint -> Maybe CPoint.CPoint
  next' = flip next

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

  imageEmbedding :: CPoint.CPoint -> Embedding -> Maybe CPoint.CPoint
  imageEmbedding = lookupEmbedding

  updateEmbedding :: CPoint.CPoint -> CPoint.CPoint -> Embedding -> Embedding
  updateEmbedding cp cp' e = case Map.lookup cp e of
                               Nothing -> Map.insert cp cp' e
                               Just _  -> Map.update (\ _ -> Just cp') cp e

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

  qQueryLeftmostAccess :: Color.Color -> State -> Maybe CPoint.CPoint
  qQueryLeftmostAccess c = queryAccess c . qLeftMost

  mkRightmostAccess :: [CPoint.CPoint] -> Access
  mkRightmostAccess = Foldable.foldr f IntMap.empty
   where
     f cp m = case IntMap.lookup (CPoint.color cp) m of
                Nothing -> IntMap.insert c cp m
                Just _  -> m

  updateRightmostAccess :: CPoint.CPoint -> Access -> Access
  updateRightmostAccess cp m = m'
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
  mkState q = State { pCPoints    = []
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

  -- Find the leftmost colored point of permutation q that is compatible
  -- to both cp and the current embedding.
  qFindLeftMost :: CPoint.CPoint -> State -> Maybe CPoint.CPoint
  qFindLeftMost cp s = case utmostLookup c s of
                               Nothing  -> firstLookup c s
                               Just cp' -> nextQ (image cp' s) s
    where
      c = CPoint.color cp

  {-|
    Transform an embedding into a list of key/value pairs.
  -}
  embeddingToList :: State -> [(CPoint.CPoint, CPoint.CPoint)]
  embeddingToList = Embedding.embeddingToList . embedding

  xResolve :: CPoint.CPoint -> Int -> State -> Maybe State
  xResolve cp t s = imageEmbedding cp (embedding s) >>=
                    xJumpThreshold t (qNext s)      >>=
                    resolve cp s

  yResolve :: CPoint.CPoint -> Int -> State -> Maybe State
  yResolve cp t s = imageEmbedding cp (embedding s) >>=
                    xJumpThreshold t (qNext s)      >>=
                    resolve cp s

  resolve :: CPoint.CPoint -> State -> CPoint.CPoint -> Maybe State
  resolve pCP s qCP = resolveAux (next pCP (pNext s)) (next qCP (qNext s)) s
    where
      e  = embedding s
      e' = updateEmbedding pCP cpQ e
      s' = s { embedding = e' }

  resolveAux :: Maybe CPoint.CPoint -> Maybe CPoint.CPoint -> State -> Maybe State
  resolveAux Nothing    _          s = Just s
  resolveAux _          Nothing    s = Nothing
  resolveAux (Just pCP) (Just qCP) s = imageEmbedding pCP (embedding s) >>=
                                       resolveAux' pCP qCP s

  resolveAux' :: CPoint.CPoint -> CPoint.CPoint -> State -> CPoint.CPoint -> Maybe State
  resolveAux' pCP s qCP qCP'
    | CPoint.xCoord qCP <= CPoint.xCoord qCP' = Just s
    | otherwise                               = resolve pCP s' qCP
