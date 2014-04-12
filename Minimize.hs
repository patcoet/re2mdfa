module Minimize where

import Data.Set (Set)
import qualified Data.Set as Set
  (Set, findMin, fromList, isSubsetOf, member, partition, singleton, toList)

import FA

distinguishable :: State -> State -> EndStates -> Delta -> Alphabet -> Bool
distinguishable s1 s2 ends delta alphabet
  | s1 == s2 = False
  | isFinal s1 /= isFinal s2 = True
  | otherwise = or [distinguishable
    (delta' s1 a) (delta' s2 a) ends delta alphabet | a <- Set.toList alphabet]
  where isFinal state = Set.member state ends
        delta' state char = head $ Set.toList $ delta state char

partition :: FA -> Set.Set (Set.Set State)
partition (FA q alpha delta start ends) =
  Set.fromList [snd $ Set.partition (distinguishable' s) q | s <- Set.toList q]
  where distinguishable' x y = distinguishable x y ends delta alpha

minimize :: FA -> FA
minimize (FA q alpha delta start ends) = FA newQ alpha newDelta newStart newEnds
  where fa = (FA q alpha delta start ends)
        part = partition fa
        newQ = Set.fromList [Set.findMin states | states <- Set.toList part]
        newStart = head
          [Set.findMin p | p <- Set.toList part, Set.member start p]
        newEnds = Set.fromList [Set.findMin p |
          p <- Set.toList part, e <- Set.toList ends, Set.member e p]
        newDelta state char = Set.singleton $ Set.findMin $ head
          [p | p <- Set.toList part, Set.isSubsetOf (delta state char) p]