module Minimize where

import Data.Set (Set)
import qualified Data.Set as Set

type State = String
type Q = [State]
type Language = [String]
type Delta = State -> Char -> State
type StartState = State
type EndStates = [State]

type Alphabet = [Char]

type Final = State -> Bool

-- Is this taking a Language correct?
data DFA = DFA Q Language Delta StartState EndStates

instance Show DFA where
  show (DFA q lang delta start ends) = show [q, lang, [start], ends]


-- For testing
delta state char
  | state == "q0" && char == 'a' = "q1"
  | state == "q0" && char == 'b' = "q2"
  | (state == "q1" || state == "q2") && char == 'a' = "q3"
  | (state == "q1" || state == "q2") && char == 'b' = "q4"
  | (state == "q3" || state == "q4") = "q5"
  | state == "q5" = "q5"

minDelta state char
  | state == "q0" = "q1q2"
  | state == "q1q2" = "q3q4"
  | state == "q5" = "q5"

alphabet = "ab"

ends = ["q1", "q2", "q5"]

q = ["q0", "q1", "q2", "q3", "q4", "q5"]

language = ["hello", "bullwhip", "oranges", "cellar"]

begin = "q0"

dfa = DFA q ["ab"] delta begin ends
--

langToAlpha :: Language -> Alphabet
langToAlpha = Set.toList . Set.fromList . Prelude.filter (/= ' ') . unwords

distinguishable :: State -> State -> EndStates -> Delta -> Alphabet -> Bool
distinguishable s1 s2 ends delta alphabet
  | s1 == s2 = False
  | f s1 /= f s2 = True
  | otherwise = not $ null [a | a <- alphabet,
      distinguishable (delta s1 a) (delta s2 a) ends delta alphabet == True]
  where f state = not $ null (filter (== state) ends)

partition :: DFA -> [[State]]
partition (DFA q lang delta start ends) = Set.toList $ Set.fromList [Set.toList $ snd $ Set.partition (d x) (Set.fromList q) | x <- q]
  where d x y = distinguishable x y ends delta alphabet

minimize :: DFA -> DFA
minimize (DFA q lang delta start ends) = DFA newQ lang newDelta newStart newEnds
  where dfa = (DFA q lang delta start ends)
        part = partition dfa
        newQ = toStr part
        newStart = concat $ concat $ filter (elem start) part
        newEnds = toStr $ Set.toList $ Set.fromList [x | x <- part, a <- ends, elem a x == True]
        newDelta state char = concat $ toStr [x | x <- part, elem (delta state char) x == True]
        toStr x = map (filter (/= ' ')) $ map unwords x