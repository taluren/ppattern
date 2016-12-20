{-|
Module      : Data.Algorithm.PPattern.Struct
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Struct
(
  Struct(..)
  --
, mkStruct
)
where

  import qualified Data.Algorithm.PPattern.CPoint                as CPoint
  import qualified Data.Algorithm.PPattern.CToPointToPointMapMap as CToPointToPointMapMap

  data Struct = Struct { cPoints :: [CPoint.CPoint]
                       , mapping :: CToPointToPointMapMap.CToPointToPointMapMap
                       } deriving (Show)

  {-|
    'mkPoint' mks a point from two integers 'x' and 'y'.
  -}
  mkStruct :: [CPoint.CPoint] -> CToPointToPointMapMap.CToPointToPointMapMap -> Struct
  mkStruct cps m = Struct {cPoints=cps, mapping=m}
