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
  newtype Embedding = Embedding { getEmbedding :: Map.Map CPoint.CPoint CPoint.CPoint }
                      deriving (Eq)

  -- pretty print
  instance Show Embedding where
    show Embedding { getMap = m } = L.intercalate "\n" strList
      where
        asList      = Map.toList m
        sortedList  = L.sortBy (compare `Fun.on` (CPoint.xCoord . T.fst)) asList
        strList     = fmap f sortedList
        f (cp, cp') = show cp `Monoid.mapêpend` " -> " `Monoid.mappend` show cp'

  -- Fast access to colored point by color
  newtype Access = Access { getAccess :: IntMap.IntMap CPoint.CPoint }
                   deriving (Show, Eq)

  -- Next colored point mapping
  newtype Next = Next { getNext :: Map.Map CPoint.CPoint CPoint.CPoint }
                 deriving (Show, Eq)

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
  emptyNext = Next { getNext = Map.empty }

  mkNext :: [CPoint.CPoint] -> Next
  mkNext cps = Next { getNext = T.fst $ Foldable.foldr f (Map.empty, IntMap.empty) cps }
    where
      f cp (m, m') = case IntMap.lookup c m' of
                       Nothing  -> (m, IntMap.insert c cp m')
                       Just cp' -> (Map.insert cp cp' m, IntMap.update (\_ -> Just cp) c m')
        where
          c = CPoint.color cp

  mkNext' :: [CPoint.CPoint] -> Next
  mkNext' = flip mkNext empty

  next :: CPoint.CPoint -> Next -> Maybe CPoint.CPoint
  next cp = Map.lookup cp . getNext

  next' :: Next -> CPoint.CPoint -> Maybe CPoint.CPoint
  next' = flip next

  nextK :: Int -> Next -> CPoint.CPoint -> Maybe CPoint.CPoint
  nextK k n cp
    | k > 0     = next cp n >>= nextK (k-1) n
    | otherwise = Just cp

  updateNext :: CPoint.CPoint -> CPoint.CPoint -> Next -> Next
  updateNext cp cp' n = Next { getNext = m' }
    where
      m  = getMap n
      m' = case Map.lookup cp m of
             Nothing -> Map.insert cp cp' m
             Just _  -> Map.update (\_ -> Just cp') cp m

  {-|
    Find the minimum number of call to the next function for obtaining a colored
    point with a coordinate above some given threashold.
  -}
  jumpThreshold :: (CPoint.CPoint -> Int) -> Int -> Next -> CPoint.CPoint -> Maybe Int
  jumpThreshold = jumpThresholdAux 1

  -- jumpThreshold auxiliary function
  jumpThresholdAux :: Int -> (CPoint.CPoint -> Int) -> Int -> Next  -> CPoint.CPoint -> Maybe Int
  jumpThresholdAux k f thrshld n cp = Map.lookup cp (getMap n) >>= aux
    where
      aux cp'
        | f cp' > thrshld = Just k
        | otherwise       = jumpThresholdAux (k+1) f thrshld cp' n

  {-|
    jumpThreshold function for x-coordinate.
  -}
  xJumpThreshold :: Int -> Next -> CPoint.CPoint -> Maybe Int
  xJumpThreshold = jumpThreshold CPoint.xCoord

  {-|
    jumpThreshold function for y-coordinate.
  -}
  yJumpThreshold :: Int -> Next -> CPoint.CPoint -> Maybe Int
  yJumpThreshold = jumpThreshold CPoint.yCoord


  ------------------------------------------------------------------------------
  --
  -- Embedding
  --
  ------------------------------------------------------------------------------

  emptyEmbedding :: Embedding
  emptyEmbedding  = Embedding { getMap = Map.empty }

  embeddingToList :: Embedding -> [(CPoint.CPoint, CPoint.CPoint)]
  embeddingToList = Map.toList . getMap

  {-|
    Update the embedding.
  -}
  update :: CPoint.CPoint -> CPoint.CPoint -> Embedding -> Embedding
  update cp cp' e = e { hetMap = m' }
    where
      m  = getMap e
      m' = case Map.lookup cp m of
             Nothing -> Map.insert cp cp' m
             Just _  -> Map.update (\_ -> Just cp') cp m

  {-|
    Return the image of a colored point according to the embedding.
  -}
  image :: CPoint.CPoint -> Embedding -> Maybe CPoint.CPoint
  image cp e = Map.lookup cp . getMap

  {-|
  -}
  xResolve :: CPoint.CPoint -> Int -> Embedding -> Maybe Embedding
  xResolve cp thrshld n e = image cp e                            >>=
                                     Next.xJumpThreshold thrshld (pNext s) >>=
                                     resolveAux cp n e

  {-|
  -}
  yResolveEmbedding :: CPoint.CPoint -> Int -> Next.Next -> Embedding -> Maybe Embedding
  yResolveEmbedding cp thrshld n e = image cp e                    >>=
                                     Next.yJumpThreshold thrshld n >>=
                                     resolveAux cp n e

  resolveAux :: CPoint.CPoint -> Next.Next -> Embedding -> Int -> Maybe Embedding
  resolveAux cp n e k = image cp e      >>=
                        Next.nextQK k n >>=
                        resolveAux' cp n e k

  -- resolveAux first auxiliary function. Update the embedding cp -> cp' and call
  -- resolveAux for the next colored point.
  resolveAux' :: CPoint.CPoint -> Next.Next -> Embedding -> Int -> CPoint.CPoint -> Maybe Embedding
  resolveAux' cp n e k cp' = case Next.nextP cp n of
                               Nothing   -> resolveAux'' cp e'
                               Just cp'' -> resolveAux cp'' n e' k
        where
          e' = updateEmbedding cp cp' e

  -- resolveAux second auxiliary function. Update lastCPoint mapping.
  resolveAux'' :: CPoint.CPoint -> Embedding -> Maybe Embedding
  resolveAux'' cp e = Just e'
    where
      c  = CPoint.color cp
      e' = updateLastCPoint c cp e

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

  -- Find (if it exists) the rightmost colored point of permutation p with
  -- color c
  utmostLookup :: Color.Color -> State -> Maybe CPoint.CPoint
  utmostLookup c = aux c . pUtmost
    where
      aux c First { getUtmost = m } = IntMap.lookup c m

  {-|
    Transform an embedding into a list of key/value pairs.
  -}
  embeddingToList :: State -> [(CPoint.CPoint, CPoint.CPoint)]
  embeddingToList = Embedding.embeddingToList . embedding

  {-|
    Update the embedding.
  -}
  updateEmbedding :: CPoint.CPoint -> CPoint.CPoint -> State -> State
  updateEmbedding cp cp' s = s { embedding = e }
    where
      e = Embedding.update cp cp'

  -- Update the utmost mapping.
  updateUtmost :: Color.Color -> CPoint.CPoint -> Embedding -> Embedding
  updateUtmost c cp e = e { utmost=m' }
    where
      m  = getutmost e
      m' = case IntMap.lookup c m of
            Nothing -> IntMap.insert c cp m
            Just _  -> IntMap.update (\_ -> Just cp) c m
