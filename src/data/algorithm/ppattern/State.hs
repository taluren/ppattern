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

  -- * The @PPState@ type
, PPState(..)

  -- * Querying and modifying
, nextX
, nextY

  -- * Modifying
, updateLinks
, updateXCMap
, updateYCMap

  -- * Querying
, xCMaps
, yCMaps
, queryNextX
, queryNextY
)
where

  import qualified Data.Algorithm.PPattern.CMaps  as CMaps
  import qualified Data.Algorithm.PPattern.CMap   as CMap
  import qualified Data.Algorithm.PPattern.CPLink as CPLink
  import qualified Data.Algorithm.PPattern.Point  as Point
  import qualified Data.Algorithm.PPattern.Perm   as Perm
  import qualified Data.Algorithm.PPattern.Color  as Color

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
  mkState lks cms  = State { links=lks, cMaps=cms }

  xCMaps :: State -> CMap.CMap
  xCMaps = CMaps.xCMap . cMaps

  yCMaps :: State -> CMap.CMap
  yCMaps = CMaps.yCMap . cMaps

  -- updateLinks :: [CPLink.CPLink] -> State -> State
  -- updateLinks lnks state = state { links=lnks }

  updateCMaps :: CMaps.CMaps -> State -> State
  updateCMaps cms state = state { cMaps=cms }

  {-|
  -}
  afterNext :: State -> CMaps.PPCMaps -> Maybe PPState
  afterNext state (CMaps.PPCMaps (p, p', cms))
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
  nextY c p thrshld state = CMaps.nextY c p thrshld (cMaps state) >>= afterNext state

  {-|
  -}
  afterQueryNext :: State -> CMaps.PPCMaps -> Maybe PPState
  afterQueryNext state (CMaps.PPCMaps (p, p', _)) = Just $ PPState (p, p', state)

  {-|

  -}
  queryNextX :: Color.Color -> Point.Point -> Perm.T -> State -> Maybe PPState
  queryNextX c p thrshld state = CMaps.queryNextX c p thrshld (cMaps state) >>= afterQueryNext state

  {-|

  -}
  queryNextY :: Color.Color -> Point.Point -> Perm.T -> State -> Maybe PPState
  queryNextY c p thrshld state = CMaps.queryNextY c p thrshld (cMaps state) >>= afterQueryNext state

  updateLinks :: [CPLink.CPLink] -> State -> State
  updateLinks lnks state = state { links=lnks }

  updateXCMap :: CMap.CMap -> State -> State
  updateXCMap cm state = state { cMaps=cms }
    where
      cms = CMaps.updateXCMap cm $ cMaps state

  updateYCMap :: CMap.CMap -> State -> State
  updateYCMap cm state = state { cMaps=cms }
    where
      cms = CMaps.updateYCMap cm $ cMaps state
