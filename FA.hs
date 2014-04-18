module FA where

import Text.PrettyPrint.Boxes
  (Box, center1, char, hsep, left, render, text, vcat)
import Data.Set (Set, member, findMin, fromList, singleton, toList)

type State = Set Int
type Q = Set State
type Delta = State -> Char -> State
type StartState = State
type EndStates = Set State
type Alphabet = Set Char

-- Maybe have a DFA type and an NFA type instead?
data FA = FA {q :: Q,
              alpha :: Alphabet,
              delta :: Delta,
              startState :: StartState,
              endStates :: EndStates}

-- Display stuff; should maybe be in Main instead
-- TODO: Change displayed state names to look nicer? As an option in main?
leftCol :: Q -> StartState -> EndStates -> Box
leftCol q startState ends = vcat left $ [text ""]
  ++ map text [(concat ["->" | state == startState])
  ++ (concat ["*" | member state ends])
  ++ show (toList state) | state <- toList q]

table :: FA -> [Box]
table (FA q alphabet delta startState endStates) = [vcat left $ [char a] ++ 
  [text $ toString (delta x a) | x <- toList q] | a <- toList alphabet]
  where toString s = show $ toList s

instance Show FA where
  show (FA q alpha delta start ends) = render $ hsep 3 center1 $
    [leftCol q start ends] ++ table (FA q alpha delta start ends)

-- For testing
mydelta :: Delta
mydelta state char
  | state' == 0 && char == 'a' = a 1
  | state' == 0 && char == 'b' = a 2
  | (state' == 1 || state' == 2) && char == 'a' = a 3
  | (state' == 1 || state' == 2) && char == 'b' = a 4
  | (state' == 3 || state' == 4) = a 5
  | state' == 5 = a 5
  where state' = findMin state
        a n = singleton n

myalphabet = fromList ['a', 'b']

myends = fromList $ map (singleton) [1, 2, 5]

myq = fromList $ map (singleton) [0 .. 5]

mybegin = singleton 0

mydfa = FA myq myalphabet mydelta mybegin myends