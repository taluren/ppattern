{-|
Module      : Data.Algorithm.PPattern.Stack
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Stack
(
  Stack(..)
  --
, mkEmpty
, fromList
  --
, isEmpty
, size
  --
, push
, pop
, popUnsafe
, top
, topUnsafe
)
where

  import qualified Data.Foldable as Fold

  newtype Stack a = Stack [a]

  {-|
    'mkPoint' mks a point from two integers 'x' and 'y'.
  -}
  mkEmpty :: Stack a
  mkEmpty = []

  {-|
    'mkPoint' mks a point from two integers 'x' and 'y'.
  -}
  fromList :: [a] -> Stack a
  fromList = Stack

  {-|
  -}
  isEmpty :: Stack a -> Bool
  isEmpty (Stack []) = True
  isEmpty _          = False

  {-|
  -}
  size :: Stack a -> Int
  size (Stack xs) = Fold.length xs

  {-|
  -}
  push :: Stack a -> a -> Stack a
  push (Stack xs) x = Stack x:xs

  {-|
  -}
  pop :: Stack a -> Maybe (Stack a)
  pop (Stack [])     = Nothing
  pop (Stack (_:xs)) = Just (Stack xs)

  {-|
  -}
  popUnsafe :: Stack a -> Stack a
  popUnsafe (Stack (_:xs)) = Stack xs

  {-|
  -}
  top :: Stack a -> Maybe a
  top (Stack [])     = Nothing
  top (Stack (x:xs)) = Just x

  {-|
  -}
  topUnsafe :: Stack a -> a
  top (Stack (x:_)) = x
