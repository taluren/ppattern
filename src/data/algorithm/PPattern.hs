{-|
Module      : Data.Algorithm.PPattern
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern
(
  search
)
where

  import qualified Data.List     as L
  import qualified System.Random.Shuffle as Random.Shuffle
  --import qualified Control.Monad.Random.Class.MonadRandom as MonadRandom

  search :: [a] -> [a] -> Bool
  search p q = not (null p') && not (null q')
    where
      p' = Random.Shuffle.shuffleM p
      q' = Random.Shuffle.shuffleM q
