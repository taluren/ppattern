{-|
Module      : Data.Algorithm.PPattern.Partition
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Partition
(

)
where

  import qualified Data.Map.Strict as Map

  import qualified Data.Algorithm.PPattern.Permutation as Permutation
  import qualified Data.Algorithm.PPattern.Point       as Point


  data NTree = NTree { size   :: {-# UNPACK #-} !Int,
                     , perms  :: [Perm.Perm]
                     , nTrees :: [NTree]
                     }
             | NLeaf { size   :: {-# UNPACK #-} !Int,
                     , perms  :: [Perm.Perm]
                     }
             deriving (Show)
