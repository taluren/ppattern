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
  import qualified Data.Algorithm.PPattern.Action as Action

  data State = State { links :: [CPLink.CPLink] -- ^ occurrence mapping
                     , cMaps :: CMaps.CMaps     -- ^ x/y-next maps
                     } deriving (Show)

  newtype PPState = PPState (Point.Point, Point.Point, State)
                    deriving (Show)

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
  afterNext :: State -> PPCMaps -> Maybe PPState
  afterNext state (PPCMaps (p, p', cms))
    | p == p'   = Just $ PPState (p, p', state)
    | otherwise = Just $ PPState (p, p', state')
    where
      state' = updateCMaps cms state

  {-|
  -}
  nextX :: Color.Color -> Point.Point -> Perm.T -> State -> Maybe PPState
  nextX c p thrshld state = CMaps.nextX c p thrshld (cMaps state) >>= afterNext state

  {-|
  -}
  nextY :: Color.Color -> Point.Point -> Perm.T -> State ->  Maybe PPState
  nextY c p thrshld state = CMaps.nextY c pval (cMaps state) >>= afterNext state

  {-|
  -}
  afterQueryNext :: State -> PPCMaps -> Maybe PPState
  afterQueryNext state (PPCMaps (p, p', _)) = Just $ PPState (p, p', state)

  {-|

  -}
  queryNextX :: Color.Color -> Point.Point -> Perm.T -> State -> Maybe PPState
  queryNextY c p thrshld state = CMaps.queryNextX c p thrshld (cMaps state) >>= afterQueryNext state

  {-|

  -}
  queryNextY :: Color.Color -> Point.Point -> Perm.T -> State -> Maybe PPState
  queryNextY c p thrshld state = CMaps.queryNextY c p thrshld (cMaps state) >>= afterQueryNext state
