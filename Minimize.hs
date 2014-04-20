module Minimize where

import Data.Set
  (Set, filter, fromList, isSubsetOf, member, partition, toList, unions)
import Prelude hiding (filter)

import FA

reachedBy :: State -> Delta -> Alphabet -> Set State
reachedBy state delta alphabet =
  fromList [delta state c | c <- toList alphabet]

parentsOf :: State -> FA -> Set State
parentsOf state fa = filter (/= state) $ fromList
  [p | p <- toList (q fa), member state (reachedBy p (delta fa) (alpha fa))]

reachable :: State -> FA -> Bool
reachable state fa
  | state == startState fa = True
  | otherwise = member state (reachedBy (startState fa) (delta fa) (alpha fa))
    || or [reachable p fa | p <- toList (parentsOf state fa)]

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

minimize :: FA -> FA
minimize (FA q alpha delta start ends) = FA q' alpha delta' start' ends'
  where fa = FA q alpha delta start ends
        q' = part $ FA (filter (\x -> reachable x fa) q) alpha delta start ends
        delta' state char = head
          [p | p <- toList q', isSubsetOf (delta state char) p]
        start' = head [p | p <- toList q', isSubsetOf start p]
        ends' = fromList
          [p | p <- toList q', end <- toList ends, isSubsetOf end p]