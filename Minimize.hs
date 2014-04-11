module Minimize where

import Data.Set (Set)
import qualified Data.Set as Set (Set, fromList, partition, toList)

import FA

distinguishable :: State -> State -> EndStates -> Delta -> Alphabet -> Bool
distinguishable s1 s2 ends delta alphabet
  | s1 == s2 = False
  | f s1 /= f s2 = True
  | otherwise = not $ null [a | a <- (Set.toList alphabet),
      distinguishable (concat (delta s1 a)) (concat (delta s2 a)) ends delta alphabet == True]
  where f state = not $ null (filter (== state) ends)

partition :: FA -> [[State]]
partition (FA q alpha delta start ends) = Set.toList $ Set.fromList [Set.toList $ snd $ Set.partition (d x) (Set.fromList q) | x <- q]
  where d x y = distinguishable x y ends delta alpha

minimize :: FA -> FA
minimize (FA q alpha delta start ends) = FA newQ alpha newDelta newStart newEnds
  where fa = (FA q alpha delta start ends)
        part = partition fa
        newQ = toStr part
        newStart = concat $ concat $ filter (elem start) part
        newEnds = toStr $ Set.toList $ Set.fromList [x | x <- part, a <- ends, elem a x == True]
        newDelta state char = toStr [x | x <- part, elem (concat (delta (take 4 state) char)) x == True]
        toStr x = map (filter (/= ' ')) $ map unwords x