{-|
Module      : Data.Algorithm.PPattern.Trgt
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Trgt
(
  -- * The @Trgt@ type
  Trgt(..)
, mkTrgt

-- * Querying
, nbColors
)
where

  import qualified Data.Algorithm.PPattern.CPoint as CPoint
  import qualified Data.Algorithm.PPattern.CMaps  as CMaps

  -- | Trgt data
  data Trgt = Trgt { cPoints :: [CPoint.CPoint]
                   , cMaps   :: CMaps.CMaps
                   } deriving (Show)

  {-|
    'mkTrgt' makes a target from a list of colored points 'cps', the x-coordinate
    next mapping and the y-coordinate next mapping.
  -}
  mkTrgt :: [CPoint.CPoint] -> CMaps.CMaps -> Trgt
  mkTrgt cps cms = Trgt {cPoints=cps, cMaps=cms}

  {-|
    The 'nbColors' function returns the number of colors in the target.
  -}
  nbColors :: Trgt -> Int
  nbColors = CMap.nbColors . xCMap . cMaps
