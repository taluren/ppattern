{-|
Module      : Data.Algorithm.PPattern.Types
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Types
(
  Length
, Index
, T
)

where

  import qualified Data.Word as Word

  -- |Integer length type
  type Length = Int

  -- |Integer index type
  type Index = Int

  -- |Integer length type
  type T = Word.Word8
