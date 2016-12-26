{-|
Module      : Data.Algorithm.PPattern.CMaps
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.CMaps
(
  -- * The @Trgt@ type
  CMaps(..)
, mkCMaps

  -- * The @PPCMaps@ type
, PPCMaps(..)

  -- * Querying & modifying
, nextX
, nextY
, nbColors

  -- * Modifying
, updateXCMap
, updateYCMap

  -- * Querying
, queryNextX
, queryNextY
)
where

  import qualified Data.Algorithm.PPattern.CMap  as CMap
  import qualified Data.Algorithm.PPattern.Perm  as Perm
  import qualified Data.Algorithm.PPattern.Point as Point
  import qualified Data.Algorithm.PPattern.Color as Color

  -- | Trgt data
  data CMaps = CMaps { xCMap :: CMap.CMap
                     , yCMap :: CMap.CMap
                     } deriving (Show)

  -- Propagating update data
  newtype PPCMaps = PPCMaps (Point.Point, Point.Point, CMaps)
                    deriving (Show)

  {-|
    'mkTrgt' makes a target from a list of colored points 'cps', the x-coordinate
    next mapping and the y-coordinate next mapping.
  -}
  mkCMaps :: CMap.CMap -> CMap.CMap -> CMaps
  mkCMaps xcm ycm = CMaps {xCMap=xcm, yCMap=ycm}

  updateXCMap :: CMap.CMap -> CMaps -> CMaps
  updateXCMap xcm cms = cms { xCMap=xcm }

  updateYCMap :: CMap.CMap -> CMaps -> CMaps
  updateYCMap ycm cms = cms { yCMap=ycm }

  afterNextX :: CMaps -> CMap.PPCMap -> Maybe PPCMaps
  afterNextX cms (CMap.PPCMap (p, p', xcm))
    | p == p'   = Just $ PPCMaps (p, p', cms)
    | otherwise = Just $ PPCMaps (p, p', cms')
    where
      cms' = updateXCMap xcm cms

  nextX :: Color.Color -> Point.Point -> Perm.T -> CMaps -> Maybe PPCMaps
  nextX c p thrshld cms = CMap.next c p thrshld (xCMap cms) >>= afterNextX cms

  afterNextY :: CMaps -> CMap.PPCMap -> Maybe PPCMaps
  afterNextY cms (CMap.PPCMap (p, p', ycm))
    | p == p'   = Just $ PPCMaps (p, p', cms)
    | otherwise = Just $ PPCMaps (p, p', cms')
    where
      cms' = updateYCMap ycm cms

  nextY :: Color.Color -> Point.Point -> Perm.T -> CMaps -> Maybe PPCMaps
  nextY c p thrshld cms = CMap.next c p thrshld (xCMap cms) >>= afterNextY cms

  {-|
  -}
  afterQueryNext :: CMaps -> CMap.PPCMap -> Maybe PPCMaps
  afterQueryNext cms (CMap.PPCMap (p, p', _)) = Just $ PPCMaps (p, p', cms)

  {-|

  -}
  queryNextX :: Color.Color -> Point.Point -> Perm.T -> CMaps -> Maybe PPCMaps
  queryNextX c p thrshld cms = CMap.queryNext c p thrshld (xCMap cms) >>= afterQueryNext cms

  {-|

  -}
  queryNextY :: Color.Color -> Point.Point -> Perm.T -> CMaps -> Maybe PPCMaps
  queryNextY c p thrshld cms = CMap.queryNext c p thrshld (yCMap cms) >>= afterQueryNext cms

  {-|
  -}
  nbColors :: CMaps -> Int
  nbColors = CMap.nbColors . xCMap
