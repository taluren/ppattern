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

  import qualified Data.Algorithm.PPattern.CPoint            as CPoint
  import qualified Data.Algorithm.PPattern.CPointPointMapMap as CPointPointMapMap

  data Struct = Struct { cPoints :: [CPoint.CPoint]
                       , mapping :: CPointPointMapMap.CPointPointMapMap
                       } deriving (Show)

  {-|
    'mkPoint' mks a point from two integers 'x' and 'y'.
  -}
  mkStruct :: [CPoint.CPoint] -> CPointPointMapMap.CPointPointMapMap -> Struct
  mkStruct cps m = Struct {cPoints=cps, mapping=m}
