{-|
Module      : Data.Algorithm.PPattern.State
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.State
(
  -- * The @State@ type
  State(..)
, mkState

-- * Querying
, next
, nextWithUpdate
)
where

  import qualified Data.Map.Strict as Map

  import qualified Data.Algorithm.PPattern.Point      as Point
  import qualified Data.Algorithm.PPattern.ColorMap   as ColorMap
  import qualified Data.Algorithm.PPattern.CPointLink as CPointLink

  data State = State { nextMaps :: ColorMap.ColorMap       -- ^ next maps
                     , links    :: [CPointLink.CPointLink] -- ^ occurrence mapping
                     } deriving (Show)

  {-|
    The 'mkState' function constructs a new state from the color-next functions
    and the occurrence mapping.
  -}
  mkState :: ColorMap.ColorMap -> [CPointLink.CPointLink] -> State
  mkState n ls = State {next=s, links=ls}

  {-|
  -}
  next :: Color.Color -> Point.Point -> State -> Maybe Point.Point
  next c p (State {nextMaps=m}) = ColorMap.next c p m

  {-|
  -}
  nextWithUpdate :: Color.Color -> Point.Point -> Permutation.T -> State -> Maybe (Point.Point, State)
  nextWithUpdate c p (State {nextMaps=m, links=ls}) = aux (ColorMap.nextWithUpdate c p m)
    where
      aux Nothing       = Nothing
      aux (Just (p, m)) = Just (p, mkState m ls)
