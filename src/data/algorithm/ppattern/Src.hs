{-|
Module      : Data.Algorithm.PPattern.Src
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Src
(
  Src
)
where

  import qualified Data.List       as L
  import qualified Data.Map.Strict as Map

  import qualified Data.Algorithm.PPattern.ColorMap as ColorMap

  type Src = ColorMap.ColorMap

  data Trgt = Trgt { cPoints :: [CPoint.CPoint]
                   , next    :: ColorMap.ColorMap
                   } deriving (Show)

  {-|
    'mkPoint' mks a point from two integers 'x' and 'y'.
  -}
  mkStruct :: [CPoint.CPoint] -> ColorMap.ColorMap -> Struct
  mkStruct cps m = Struct {cPoints=cps, next=m}
