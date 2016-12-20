{-|
Module      : Data.Algorithm.PPattern.CToPointToPointMapMapMap
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.CToPointToPointMapMap
(
  CToPointToPointMapMap
)
where

  import qualified Data.Map.Strict as Map

  import qualified Data.Algorithm.PPattern.PointToPointMap as PointToPointMap
  import qualified Data.Algorithm.PPattern.Color           as Color


  type CToPointToPointMapMap = Map.Map Color.Color PointToPointMap.PointToPointMap
