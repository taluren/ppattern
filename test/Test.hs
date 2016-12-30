import qualified Data.List as L
import qualified Data.Foldable as Foldable
import Data.Function

s :: (Ord a) => [[a]] -> [[a]]
s = Foldable.concatMap L.sort . fmap (fmap snd) . L.groupBy (\xs ys -> fst xs == fst ys) . L.sortBy (compare `on` fst) . fmap (\ys -> (Foldable.length ys, ys))
-- s = Foldable.concatMap L.sort . L.groupBy (\l1 l2 -> Foldable.length l1 == Foldable.length l2) . L.sortOn Foldable.length
