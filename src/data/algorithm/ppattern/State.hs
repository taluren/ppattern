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
)
where

  import qualified Data.Algorithm.PPattern.ColorMap   as ColorMap
  import qualified Data.Algorithm.PPattern.CPointLink as CPointLink

  data State = State { xMaps :: ColorMap.ColorMap       -- ^ x-next maps
                     , yMaps :: ColorMap.ColorMap       -- ^ y-next maps
                     , links :: [CPointLink.CPointLink] -- ^ occurrence mapping
                     } deriving (Show)

  {-|
    The 'mkState' function constructs a new state from the color-next functions
    and the occurrence mapping.
  -}
  mkState :: ColorMap.ColorMap -> [CPointLink.CPointLink] -> State
  mkState xms yms lks = State {xMaps=m, yMaps=yms, links=lks}
