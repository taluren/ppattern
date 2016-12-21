{-|
Module      : Data.Algorithm.PPattern.PointMap
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.PointMap
(
  PointMap
)
where

  import qualified Data.Map.Strict as Map

  import qualified Data.Algorithm.PPattern.Point as Point

  type PointMap = Map.Map Point.Point Point.Point
