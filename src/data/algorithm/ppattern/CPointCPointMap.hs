{-|
Module      : Data.Algorithm.PPattern.CPointCPointMap
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.CPointCPointMap
(
  CPointPointMap
)
where

  import qualified Data.Map.Strict as Map

  import qualified Data.Algorithm.PPattern.PointPointMap as PointPointMap


  type CPointPointMap = Map.Map C PointPointMap.PointPointMap