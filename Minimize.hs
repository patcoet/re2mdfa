module Minimize where

import Data.Set (Set, empty, fromList, insert, isSubsetOf, member, partition,
                toList, unions)

import FA

distinguishable :: State -> State -> EndStates -> Delta -> Alphabet -> Bool
distinguishable s1 s2 ends delta alphabet
  | s1 == s2 = False
  | isFinal s1 /= isFinal s2 = True
  | otherwise = or [distinguishable
    (delta s1 a) (delta s2 a) ends delta alphabet | a <- toList alphabet]
  where isFinal state = member state ends
        delta' state char = head $ toList $ delta state char

part :: FA -> Set State
part (FA q alpha delta start ends) = fromList
  [unions $ toList $ snd $ partition (d s) q | s <- toList q]
  where d x y = distinguishable x y ends delta alpha

reachableStates :: FA -> Set State
reachableStates (FA _ alpha d start _) = reachableStates' d start alpha empty

reachableStates' :: Delta -> State -> Alphabet -> Set State -> Set State
reachableStates' delta state alphabet visitedStates
  | member state visitedStates = empty
  | children == [] = fromList [state]
  | otherwise = insert state $ unions 
    [reachableStates' delta child alphabet visitedStates' | child <- children]
  where children = filter (/= state) [delta state c | c <- toList alphabet]
        visitedStates' = insert state visitedStates

minimize :: FA -> FA
minimize (FA q alpha delta start ends) = FA q' alpha delta' start' ends'
  where fa = FA q alpha delta start ends
        q' = part $ FA (reachableStates fa) alpha delta start ends
        delta' state char = head
          [p | p <- toList q', isSubsetOf (delta state char) p]
        start' = head [p | p <- toList q', isSubsetOf start p]
        ends' = fromList
          [p | p <- toList q', end <- toList ends, isSubsetOf end p]
