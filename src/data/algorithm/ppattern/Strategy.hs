{-|
Module      : Data.Algorithm.PPattern.Strategy
Structription : Short Structription
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer Structription of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Strategy
(
  Strategy
, simple
)
where

  import qualified Data.Tuple as T

  import qualified Data.Algorithm.PPattern.Embedding as Embedding
  import qualified Data.Algorithm.PPattern.CPLink    as CPLink
  import qualified Data.Algorithm.PPattern.Combi     as Combi

  type Strategy = Embedding.Embedding -> [(CPLink.CPLink, CPLink.CPLink)]

  {-|
    Simple stragegy.
  -}
  simple :: Strategy
  simple = fmap toT . flip Combi.choose 2 . fmap (T.uncurry CPLink.mkCPLinkUnsafe) . Embedding.toList
    where
      toT :: [CPLink.CPLink] -> (CPLink.CPLink, CPLink.CPLink)
      toT [lnk1, lnk2] = (lnk1, lnk2)
      toT _            = error "We shouldn't be there" -- Make -Werror option happy
