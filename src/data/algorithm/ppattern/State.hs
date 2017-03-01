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
  data State = State { pCPoints   :: [CPoint.CPoint]
                     , qCPoint    :: [CPoint.CPoint]
                     , embedding  :: Embedding
                     , pRightmost :: Access
                     , qLeftmost  :: Access
                     , pNext      :: Next
                     , qNext      :: Next
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
  jumpThreshold coord t n cp = aux (next cp n)
    where
      aux Nothing = Nothing
      aux (Just cp')
        | coord c > t = Just cp'
        | otherwise   = aux (next cp' n)

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
  lookupNext = Map.lookup

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
  emptyAccess = Access { getAccess = IntMap.empty }

  queryAccess :: Color.Color -> Access -> Maybe CPoint.CPoint
  queryAccess c Access { getAccess = m } = IntMap.lookup c m

  mkLeftmost :: [CPoint.CPoint] -> Access
  mkLeftmost cps = Access { getAccess = Foldable.foldl f IntMap.empty cps }
    where
      f m cp = case IntMap.lookup (CPoint.color cp) m of
                 Nothing -> IntMap.insert c cp m
                 Just _  -> m

  qQueryLeftmost :: Color.Color -> State -> Maybe CPoint.CPoint
  qQueryLeftmost c = queryAccess c . qLeftmost

  mkRightmost :: [CPoint.CPoint] -> Access
  mkRightmost cps = Access { getAccess = Foldable.foldr f IntMap.empty cps }
   where
     f cp m = case IntMap.lookup (CPoint.color cp) m of
                Nothing -> IntMap.insert c cp m
                Just _  -> m

  updateRightmost :: CPoint.CPoint -> Access -> Access
  updateRightmost cp Access { getAccess = m } = Access { getAccess = m' }
    where
      m' = case IntMap.lookup (CPoint.color cp) m of
             Nothing -> IntMap.insert c cp m
             Just _  -> m

  pQueryRightmost :: Color.Color -> State -> Maybe CPoint.CPoint
  pQueryRightmost c = queryAccess c . pRightmost

  ------------------------------------------------------------------------------
  --
  -- State
  --
  ------------------------------------------------------------------------------

  {-|
    Make a new state. Permutation q is required.
  -}
  mkState :: Perm.Perm -> State
  mkState q = State { pCPoints   = []
                    , qCPoint    = cps
                    , embedding  = emptyEmbedding
                    , utmost     = IntMap.empty
                    , lastCPoint = Nothing
                    , pNext      = emptyNext
                    , qNext      = n
                    }
    where
      cps = mkQ q
      n   = mkNext cps

  -- construct the colored points list associated to permutation q using
  -- a greedy coloring procedure.
  mkQ :: Perm.Perm -> [CPoint.CPoint]
  mkQ q = mkQAux [] (Perm.index q) IntMap.empty

  -- mkQ auxiliary function.
  mkQAux :: [CPoint.CPoint] -> [(Int, Int)] -> IntMap.IntMap Int -> [CPoint.CPoint]
  mkQAux acc [] _             = L.reverse acc
  mkQAux acc ((x, y) : xys) m = mkQAux (cp : acc) xys m'
    where
      -- Find the smallest color that point (x, y) can accept
      c  = findSmallestColor y m

      -- Update the map accordingly
      m' = updateMap c y m

      -- Construct the associated colored point
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
  appendCPoint :: CPoint.CPoint -> State -> Maybe State
  appendCPoint cp s = findLeftMostQCPoint cp s >>= appendCPointAux cp s

  appendCPointAux :: CPoint.CPoint -> State -> CPoint.CPoint -> Maybe State
  appendCPointAux cp s cp' =  s { pCPoints  = pCPoints s `Mondoid.mappend` [cp]
                                , embedding = updateEmbedding cp cp' (embedding s)
                                , utmost    = updateUtmost cp
                                , lastCPoint = image
                                , pNext     = Next.updateP cp cp' (nextP s)
                                }
    where
      cps  = pCPoints s `Mondoid.mappend` [cp]
      e    = updateEmbedding cp cp' (embedding s)
      u    = updateUtmost cp (utmost s)
      cp'' = image cp e
      n    = updateNext cp cp' (nextP s)

  -- Find the leftmost colored point of permutation q that is compatible
  -- to both cp and the current embedding.
  findLeftMostQCPoint :: CPoint.CPoint -> State -> Maybe CPoint.CPoint
  findLeftMostQCPoint cp s = case utmostLookup c s of
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
