{-|
Module      : Data.Algorithm.PPattern.Statistics
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Statistics
(
  average
)
where

  import qualified Data.List  as L

  {-|
    The 'square' function squares an integer.
    It takes one argument, of type 'Int'.
  -}
  average xs = realToFrac (sum xs) / L.genericLength xs
