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

  import qualified Data.List       as L
  import qualified Data.Map.Strict as M
  import Control.Monad

  import Data.Algorithm.PPattern.ColSeq
  import Data.Algorithm.PPattern.ColPoint

  data ColLink a c = Link { src  :: ColPoint a c
                          , srgt :: ColPoint a c
                          } deriving (Show, Eq)

  newtype ColMapping a c = ColMapping [ColLink a c]

  {-|
    'leftmostColMapping p q' return the leftmost color friendly mapping from 'p'
    to 'a'.
  -}
  leftmostColMapping :: ColSeq a c -> ColSeq a c -> Maybe (ColMapping a c)
  leftmostColMapping p q = aux (ColSeq.toList p) (ColSeq.toList q)
    where
      aux :: [ColPoint a c] -> [ColPoint a c] -> Maybe (ColMapping a c)
      aux [] []          = Just []
      aux [] _           = Just []
      aux _  []          = Nothing
      aux (p:ps) (q:qs)
        | col p == col q = fmap (ColLink {src=p, trgt=q, col=col p}:) aux ps qs
        | otherwise      = aux (p:ps) qs

  {-|
    The 'square' function squares an integer.
    It takes one argument, of type 'Int'.
  -}
  search :: ColSeq a c -> ColSeq a c -> Maybe (ColMapping a c)
  search p q = leftmostColMapping p q >>= aux
    where
      aux cm = aux' (resolve1 p q cm >>= resolve2 p q)
        where
          aux' Nothing  = Nothong
          aux (Just cm')
            | cm /= cm' = aux cm'
            | otherwise = cm'

  {-|
    The 'square' function squares an integer.
    It takes one argument, of type 'Int'.
  -}
  -- resolve1 :: ColMapping a c -> Maybe (ColMapping a c)
  -- resolve1 []        = Just []
  -- resolve1 [l]       = Just [l]
  -- resolve1 (l:l':ls)
  --   | conflict l l' = fmap (l:) (resolve1 (update1 l':ls))
  --   | otherwise     = fmap (l:) (resolve1 l':ls)
  --   where
  --     bichromatic l l'   = colour l /= colour l'
  --     orderConflict l l' = (sPoint l) `southEastDomination` (tPoint l')
  --     confict l l'       = bichromatic l l' && orderConflict l l'
