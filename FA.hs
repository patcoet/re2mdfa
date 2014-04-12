module FA where

import Text.PrettyPrint.Boxes
  (Box, center1, char, hsep, left, render, text, vcat)
import Data.List (isInfixOf)
import qualified Data.Set as Set
  (Set, member, findMax, findMin, fromList, map, singleton, toList)

type State = Set.Set Int
type Q = Set.Set State
type Delta = State -> Char -> Set.Set State
type StartState = State
type EndStates = Set.Set State
type Alphabet = Set.Set Char

type Final = State -> Bool

-- Maybe have a DFA type and an NFA type instead?
data FA = FA {q :: Q,
              alpha :: Alphabet,
              delta :: Delta,
              startState :: StartState,
              endStates :: EndStates}

-- Make sure state names don't overlap
rename :: (FA, FA) -> (FA, FA)
rename (fa1, fa2)
  | Set.findMax (q1) >= Set.findMin (q2)
              = (fa1, FA (q2') (alpha fa2) d2' start2' end2')
  | otherwise = (fa1, fa2)
  where add x          = Set.map (+ x)
        q1             = Set.findMin $ q fa1
        q2             = Set.findMin $ q fa2
        diff           = Set.findMax q1 + 1 - Set.findMin q2
        q2'            = Set.singleton $ add diff q2
        d2 state char  = (delta fa2) (Set.map (diff -) (Set.singleton state)) char
        d2' state char = Set.map (add diff) (d2 (Set.findMin state) char)
        start2'        = Set.map (diff +) (startState fa2)
        end2'          = Set.map (add diff) (endStates fa2)

-- Display stuff; should maybe be in Main instead
-- TODO: Change displayed state names to look nicer? As an option in main?
leftCol :: Q -> StartState -> EndStates -> Box
leftCol q startState ends = vcat left $ [text ""]
  ++ map text [(concat ["->" | state == startState])
  ++ (concat ["*" | Set.member state ends])
  ++ show state | state <- Set.toList q]

table :: FA -> [Box]
table (FA q alphabet delta startState endStates) = [vcat left $ [char a] ++ 
  [text $ toString (delta x a) | x <- Set.toList q] | a <- Set.toList alphabet]
  where toString s = concat $ map show $ Set.toList s

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
  where state' = Set.findMin state
        a n = Set.singleton $ Set.singleton n

myalphabet = Set.fromList ['a', 'b']

myends = Set.singleton $ Set.fromList [1, 2, 5]

myq = Set.singleton $ Set.fromList [0 .. 5]

mybegin = Set.singleton 0

mydfa = FA myq myalphabet mydelta mybegin myends