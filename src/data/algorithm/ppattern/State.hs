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
  State
)
where

  import qualified Data.Map.Strict as Map

  import qualified Data.Algorithm.PPattern.Struct           as Struct
  import qualified Data.Algorithm.PPattern.CPointCPointLink as CPointCPointLink

  data State = State { src   :: Struct.Struct
                     , trgt  :: Struct.Struct
                     , links :: [CPointCPointLink.CPointCPointLink]
                     } deriving (Show)

  {-|

  -}
  mkState :: Struct.Struct -> Struct.Struct -> [CPointCPointLink.CPointCPointLink] -> State
  mkState s t ls = State {src=s, trgt=t, links=ls} 
