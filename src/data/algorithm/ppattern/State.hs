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

-- * Modifying
, pAppend
, xResolve
, yResolve

  -- * The @Embeddingp@ type
, Embedding
)
where

  import qualified Data.List          as L
  import qualified Data.Set           as Set
  import qualified Data.Tuple         as T
  import qualified Data.Function      as Fun
  import qualified Data.Map.Strict    as Map
  import qualified Data.IntMap.Strict as IntMap
  import qualified Data.Monoid        as Monoid
  import qualified Data.Foldable      as Foldable

  import qualified Data.Algorithm.PPattern.Perm   as Perm
  import qualified Data.Algorithm.PPattern.CPoint as CPoint
  import qualified Data.Algorithm.PPattern.Color  as Color

  -- colored point to colored points mapping
  type Embedding = Map.Map CPoint.CPoint CPoint.CPoint

  -- Fast access to colored point by color
  type Access = IntMap.IntMap CPoint.CPoint

  -- Colored point to colored point mapping
  type Next = Map.Map CPoint.CPoint CPoint.CPoint

  -- The state of a search
  data State =
    State { pCPoints                :: [CPoint.CPoint]
          , qCPoints                :: [CPoint.CPoint]
          , embedding               :: Embedding
          , pRightmostMappedByColor :: Access
          , qLeftmostByColor        :: Access
          , qRightmostMappedByColor :: Access
          , qRightmost              :: Maybe CPoint.CPoint
          , pNext                   :: Next
          , qNext                   :: Next
          } deriving (Eq)

  -- Show class
  instance Show State where
    show State { embedding = e } = show e

  ------------------------------------------------------------------------------
  --
  -- Next
  --
  ------------------------------------------------------------------------------

  emptyNext :: Next
  emptyNext = Map.empty

  mkNext :: [CPoint.CPoint] -> Next
  mkNext = T.fst . Foldable.foldr f (emptyNext, IntMap.empty)
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
  updateNext cp cp' n = case Map.lookup cp n of
                          Nothing -> Map.insert cp cp' n
                          Just _  -> Map.update (\_ -> Just cp') cp n

  jumpThreshold :: (CPoint.CPoint -> Int) ->
                   Int                    ->
                   Next                   ->
                   CPoint.CPoint          ->
                   Maybe CPoint.CPoint
  jumpThreshold f t n cp = aux (lookupNext cp n)
    where
      aux Nothing = Nothing
      aux (Just cp')
        | f cp' > t = Just cp'               -- above threshold, done.
        | otherwise = aux (lookupNext cp' n) -- below threshold, keep on searching.

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

  lookupAccess :: Color.Color -> Access -> Maybe CPoint.CPoint
  lookupAccess = IntMap.lookup

  updateAccess :: Color.Color -> CPoint.CPoint -> Access -> Access
  updateAccess c cp a = case IntMap.lookup c a of
                          Nothing -> IntMap.insert c cp a
                          Just _  -> IntMap.update (\ _ -> Just cp) c a

  mkLeftmostByColor :: [CPoint.CPoint] -> Access
  mkLeftmostByColor = Foldable.foldl f IntMap.empty
    where
      f m cp = case IntMap.lookup (CPoint.color cp) m of
                 Nothing -> IntMap.insert (CPoint.color cp) cp m
                 Just _  -> m

  updateRightmostByColor :: CPoint.CPoint -> Access -> Access
  updateRightmostByColor cp m = m'
    where
      m' = case IntMap.lookup (CPoint.color cp) m of
             Nothing -> IntMap.insert (CPoint.color cp) cp m
             Just _  -> m

  ------------------------------------------------------------------------------
  --
  -- State
  --
  ------------------------------------------------------------------------------

  {-|
    Make a new state. Permutation q is required.
  -}
  mkState :: Perm.Perm -> State
  mkState q  = State { pCPoints                = []
                     , qCPoints                = qcps
                     , embedding               = emptyEmbedding
                     , pRightmostMappedByColor = emptyAccess
                     , qLeftmostByColor        = mkLeftmostByColor qcps
                     , qRightmostMappedByColor = emptyAccess
                     , qRightmost              = Nothing
                     , pNext                   = emptyNext
                     , qNext                   = n
                     }
    where
      qcps = mkQ q
      n    = mkNext qcps

  -- construct the colored points list associated to permutation q using
  -- a greedy coloring procedure.
  mkQ :: Perm.Perm -> [CPoint.CPoint]
  mkQ q = mkQAux [] (Perm.index q) IntMap.empty

  -- mkQ auxiliary function. Greeding increasing coloring.
  mkQAux :: [CPoint.CPoint] -> [(Int, Int)] -> IntMap.IntMap Int -> [CPoint.CPoint]
  mkQAux acc []             _ = L.reverse acc
  mkQAux acc ((x, y) : xys) m = mkQAux (cp : acc) xys m'
    where
      c  = findSmallestColor y m
      m' = case IntMap.lookup c m of
             Nothing -> IntMap.insert c y m
             Just _  -> IntMap.update (\_ -> Just y) c m
      cp = CPoint.mkCPoint x y c

  -- Auxialiary function for mkQAux.
  -- Find the smallest color for a new y-coordinate
  findSmallestColor :: Int -> IntMap.IntMap Int -> Int
  findSmallestColor y m = aux 1
    where
      aux c = case IntMap.lookup c m of
                Nothing -> c
                Just y' -> if y' < y then c else aux (c+1)

  {-|
    The colors in permutation q.
  -}
  qColors :: State -> [Color.Color]
  qColors State { qCPoints = qcps } = Set.toList . Set.fromList $ fmap CPoint.color qcps

  {-|
    Add a new colored point to the list of colored points associated
    to permutation p. Update the state accordingly.
  -}
  pAppend :: CPoint.CPoint -> State -> Maybe State
  pAppend pcp s = case lookupAccess (CPoint.color pcp) (pRightmostMappedByColor s) of
                    Nothing   -> pAppendAux1 pcp  pcp s
                    Just pcp' -> pAppendAux1 pcp' pcp s

  pAppendAux1 :: CPoint.CPoint -> CPoint.CPoint -> State -> Maybe State
  pAppendAux1 pcp pcp' s =
    case lookupEmbedding pcp (embedding s) of
      Nothing  -> lookupAccess (CPoint.color pcp) (qNext s) >>=
                  pAppendAux2 pcp pcp' s
      Just qcp -> lookupNext (CPoint.color pcp) (qLeftmostByColor s) >>=
                  pAppendAux2 pcp pcp' s

  pAppendAux2 :: CPoint.CPoint -> CPoint.CPoint  -> State -> CPoint.CPoint -> Maybe State
  pAppendAux2 pcp pcp' s qcp =
    case qRightmost s of
      Nothing   -> pAppendAux3 pcp pcp' qcp  s qcp
      Just qcp' -> pAppendAux3 pcp pcp' qcp' s qcp

  pAppendAux3 :: CPoint.CPoint ->
                 CPoint.CPoint ->
                 CPoint.CPoint ->
                 State         ->
                 CPoint.CPoint ->
                 Maybe State
  pAppendAux3 pcp pcp' qcp s qcp'
    | x' < x    = lookupNext qcp' (qNext s) >>= pAppendAux3 pcp pcp' qcp s
    | otherwise = pAppendFinalize pcp pcp' qcp qcp' s
    where
      x  = CPoint.xCoord qcp
      x' = CPoint.xCoord qcp'

  pAppendFinalize :: CPoint.CPoint -> CPoint.CPoint -> CPoint.CPoint -> CPoint.CPoint -> State -> Maybe State
  pAppendFinalize pcp pcp' qcp qcp' s = Just s'
    where
      pCPoints'          = pCPoints s `Monoid.mappend` [pcp']
      embedding'         = updateEmbedding pcp' qcp' (embedding s)
      pRightmostMappedByColor' = updateAccess (CPoint.color pcp) pcp' (pRightmostMappedByColor s)
      qRightmost'        = qcp
      pNext'             = updateNext pcp pcp' (pNext s)

      s'   = s { pCPoints          = pCPoints'
               , embedding         = embedding'
               , pRightmostMappedByColor = pRightmostMappedByColor'
               , qRightmost        = Just qRightmost'
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
  resolve pcp s qcp = resolveAux (lookupNext pcp (pNext s)) (lookupNext qcp (qNext s)) s
    where
      embedding' = updateEmbedding pcp qcp (embedding s)

      qRightmost' =
        case qRightmost s of
                        Nothing   -> Just qcp
                        Just qcp' -> if CPoint.xCoord qcp > CPoint.xCoord qcp'
                                     then Just qcp
                                     else Just qcp'

      qRightmostMappedByColor' =
        case lookupAccess (CPoint.color qcp) (qRightmostMappedByColor s) of
          Nothing   -> updateAccess (CPoint.color qcp) qcp (qRightmostMappedByColor s)
          Just qcp' -> if CPoint.xCoord qcp > CPoint.xCoord qcp'
                       then updateAccess (CPoint.color qcp) qcp (qRightmostMappedByColor s)
                       else qRightmostMappedByColor s

      s' = s { embedding         = embedding'
             , qRightmost        = qRightmost'
             , qRightmostMappedByColor = qRightmostMappedByColor'
             }

  resolveAux :: Maybe CPoint.CPoint -> Maybe CPoint.CPoint -> State -> Maybe State
  resolveAux Nothing    _          s = Just s
  resolveAux _          Nothing    s = Nothing
  resolveAux (Just pcp) (Just qcp) s = lookupEmbedding pcp (embedding s) >>=
                                       resolveAux' pcp qcp s

  resolveAux' :: CPoint.CPoint -> CPoint.CPoint -> State -> CPoint.CPoint -> Maybe State
  resolveAux' pcp qcp s qcp'
    | CPoint.xCoord qcp <= CPoint.xCoord qcp' = Just s
    | otherwise                               = resolve pcp s qcp
