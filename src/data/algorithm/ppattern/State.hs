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

  data State = State { pCPoints  :: [CPoint.CPoint]
                     , qCPoint   :: [CPoint.CPoint]
                     , embedding :: Embedding
                     , utmost    :: IntMap.IntMap CPoint.CPoint
                     , pNext     :: Next
                     , qNext     :: Next
                     } deriving (Eq)

  instance Show State where
   show State { embedding = e } = show e

  {-|
    Make a new state. Permutation q is required.
  -}
  mkState :: Perm.Perm -> State
  mkState q = State { pCPoints  = []
                    , qCPoint   = cps
                    , embedding = Embedding.empty
                    , utmost    = IntMap.empty
                    , pNext     = Next.empty
                    , qNext     = n
                    }
    where
      cps = mkQ q
      n   = Next.mkQ cps Next.empty

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
                                , embedding = Embedding.update cp cp' (embedding s)
                                , utmost    = updateUtmost cp
                                , pNext     = Next.updateP cp cp' (nextP s)
                                }

  -- Find the leftmost colored point of permutation q that is compatible
  -- to both cp and the current embedding.
  findLeftMostQCPoint :: CPoint.CPoint -> State -> Maybe CPoint.CPoint
  findLeftMostQCPoint cp s = IntMap.lookup (CPoint.color cp) (utmost s) >>=
                             Next.next (qNext s)

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
