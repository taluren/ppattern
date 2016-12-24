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

-- * Querying
, nbColors
)
where

  import qualified Data.Algorithm.PPattern.CMap as CMap

  -- | Trgt data
  data CMaps = CMaps { xCMap :: CMap.CMap
                     , yCMap :: CMap.CMap
                     } deriving (Show)

  {-|
    'mkTrgt' makes a target from a list of colored points 'cps', the x-coordinate
    next mapping and the y-coordinate next mapping.
  -}
  mkCMaps :: CMap.CMap -> CMap.CMap -> CMaps
  mkCMaps xcm ycm = CMaps {xCMap=xcm, yCMap=ycm}

  updateXCMap :: Cmap.Cmap -> CMaps -> CMaps
  updateXCMap xcm cms = cms { xCMap=xcm }

  updateYCMap :: Cmap.Cmap -> CMaps -> CMaps
  updateYCMap ycm cms = cms { yCMap=ycm }
