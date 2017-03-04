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
  -- * The @Strategy@ type
  Strategy

  -- * strategies
, anyConflict
, orderConflictFirst
, valueConflictFirst
)
where

  import qualified Data.Foldable as Foldable

  import qualified Data.Algorithm.PPattern.State    as State
  import qualified Data.Algorithm.PPattern.CPoint   as CPoint
  import qualified Data.Algorithm.PPattern.Combi    as Combi
  import qualified Data.Algorithm.PPattern.Conflict as Conflict

  -- strategy
  type Strategy = State.State -> Maybe Conflict.Conflict

  type Link  = (CPoint.CPoint, CPoint.CPoint)
  type PLink = (Link, Link)

  -- All pairs of links
  collect :: State -> [PLink]
  collect = flip Combi.choose 2 . State.embeddingToList

  orderConflict :: Link -> Link -> Bool
  orderConflict (pcp1, qcp1) (pcp2, qcp2) = x1 < x2 && x1' > x2'
    where
      x1  = CPoint.xCoord pcp1
      x1' = CPoint.xCoord qcp1

      x2  = CPoint.xCoord pcp2
      x2' = CPoint.xCoord qcp2

  reportOrderConflict :: Link -> Link -> Bool
  reportOrderConflict (_, qcp1) (pcp2, _) = conflict
    where
      conflict = OrderConflict pcp2 (CPoint.xCoord qcp1)

  valueConflict :: Link -> Link -> Bool
  valueConflict (pcp1, qcp1) (pcp2, qcp2) = y1 < y2 && y1' > y2'
    where
      y1  = CPoint.yCoord pcp1
      y1' = CPoint.yCoord qcp1

      y2  = CPoint.yCoord pcp2
      y2' = CPoint.yCoord qcp2

  reportValueConflict :: Link -> Link -> Conflict.Conflict
  reportValueConflict (_, qcp1) (pcp2, _) = conflict
    where
      conflict = ValueConflict pcp2 (CPoint.yCoord qcp1)

  {-|
    Return any conflict. Return Nothing if there is no conflict.
  -}
  anyConflict :: Strategy
  anyConflict = aux . collect
    where
      aux [] = Nothing
      aux ((link1, link2) : plinks)
        | orderConflict link1 link2 = Just $ reportOrderConflict link1 link2
        | orderConflict link2 link1 = Just $ reportOrderConflict link2 link1
        | valueConflict link1 link2 = Just $ reportValueConflict link1 link2
        | valueConflict link2 link1 = Just $ reportValueConflict link2 link1
        | otherwise                 = aux plinks

  {-|
    Return any order conflict. If such a conflict does not exists, report any
    value conflict. Return Nothing if there is no conflict.
  -}
  orderConflictFirst :: Strategy
  orderConflictFirst = aux Nothing . collect
    where
      aux Nothing   []              = Nothing
      aux vConflict []              = vConflict
      aux vConflict ((link1, link2) : plinks)
        | orderConflict link1 link2 = Just $ reportOrderConflict link1 link2
        | orderConflict link2 link1 = Just $ reportOrderConflict link2 link1
        | valueConflict link1 link2 = case vConflict of
                                        Nothing -> aux (Just $ reportValueConflict link1 link2) plinks
                                        _       -> aux vConflict plinks

        | valueConflict link2 link1 = case vConflict of
                                        Nothing -> aux (Just $ reportValueConflict link2 link1) plinks
                                        _       -> aux vConflict plinks
        | otherwise                 = aux plinks

  {-|
    Return any order conflict. If such a conflict does not exists, report any
    value conflict. Return Nothing if there is no conflict.
  -}
  valueConflictFirst :: Strategy
  valueConflictFirst = aux Nothing . collect
    where
      aux Nothing   []              = Nothing
      aux oConflict []              = oConflict
      aux oConflict ((link1, link2) : plinks)
        | orderConflict link1 link2 = case oConflict of
                                        Nothing -> aux (Just $ reportOrderConflict link1 link2) plinks
                                        _       -> aux oConflict plinks
        | orderConflict link2 link1 = case vConflict of
                                        Nothing -> aux (Just $ reportOrderConflict link2 link1) plinks
                                        _       -> aux oConflict plinks
        | valueConflict link1 link2 = Just $ reportValueConflict link1 link2
        | valueConflict link2 link1 = Just $ reportValueConflict link2 link1

        | otherwise                 = aux plinks
