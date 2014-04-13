module Minimize where

import Data.List (isInfixOf)
import qualified Data.Set as Set
  (Set, findMin, fromList, isSubsetOf, member, partition, singleton, toList, unions)

import FA

distinguishable :: State -> State -> EndStates -> Delta -> Alphabet -> Bool
distinguishable s1 s2 ends delta alphabet
  | s1 == s2 = False
  | isFinal s1 /= isFinal s2 = True
  | otherwise = or [distinguishable
    (delta s1 a) (delta s2 a) ends delta alphabet | a <- Set.toList alphabet]
  where isFinal state = Set.member state ends
        delta' state char = head $ Set.toList $ delta state char

partition :: FA -> Set.Set State
partition (FA q alpha delta start ends) = Set.fromList
  [Set.unions $ Set.toList $ snd $ Set.partition (d s) q | s <- Set.toList q]
  where d x y = distinguishable x y ends delta alpha

minimize :: FA -> FA
minimize (FA q alpha delta start ends) = FA q' alpha delta' start' ends'
  where q' = partition (FA q alpha delta start ends)
        delta' state char = head
          [p | p <- Set.toList q',
          isInfixOf (Set.toList $ delta state char) (Set.toList p)]
        start' = head
          [p | p <- Set.toList q', isInfixOf (Set.toList start) (Set.toList p)]
        ends' = Set.fromList
          [p | p <- Set.toList q', end <- Set.toList ends,
          isInfixOf (Set.toList end) (Set.toList p)]