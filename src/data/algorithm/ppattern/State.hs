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
)
where

  import qualified Data.Algorithm.PPattern.CMaps  as CMaps
  import qualified Data.Algorithm.PPattern.CPLink as CPLink

  data State = State { links :: [CPLink.CPLink] -- ^ occurrence mapping
                     , cMaps :: CMaps.CMaps     -- ^ x/y-next maps
                     } deriving (Show)

  {-|
    The 'mkState' function constructs a new state from the color-next functions
    and the occurrence mapping.
  -}
  mkState :: [CPLink.CPLink] -> CMaps.CMaps -> State
  mkState lks cms  = State {xCMaps=xcm, yCMaps=ycm, links=lks}

  xCMaps :: State -> CMap.Cmap
  xCMaps = CMaps.xCMap . cMaps

  yCMaps :: State -> CMap.Cmap
  yCMaps = CMaps.yCMap . cMaps

  updateLinks :: [CPLink.CPLink] -> State -> State
  updateLinks lnks state = state { links=lnks }

  updateCMaps :: CMaps.CMaps -> State -> State
  updateCMaps cms state = state { cMaps=cms }

  {-|
  -}
  update :: State -> (Point.Point, CMaps.CMaps, Action.Action) -> Maybe (Point.Point, State)
  update state (p, _,   Action.Propagate) = Just (p, state)
  update state (p, cms, Action.Update)    = Just (p, state')
    where
      state' = updateCMaps cms state

  {-|
  -}
  nextX :: Color.Color -> Point.Point -> State -> Maybe (Point.Point, State)
  nextX c p state = CMaps.nextX c p (cMaps state) >>= update state

  {-|
  -}
  nextY :: Color.Color -> Point.Point -> State -> Maybe (Point.Point, State)
  nextY c p state = CMaps.nextY c p (cMaps state) >>= update state

  {-|

  -}
  queryNextX :: Color.Color -> Point.Point -> Perm.T -> State -> Maybe Point.Point
  queryNextY c p state = CMap.next c p xcm
    where
      cm  = cMaps state
      ycm = CMap.xCMap

  {-|

  -}
  queryNextY :: Color.Color -> Point.Point -> Perm.T -> State -> Maybe Point.Point
  queryNextY c p state = CMap.next c p ycm
    where
      cm  = cMaps state
      ycm = CMap.yCMap
