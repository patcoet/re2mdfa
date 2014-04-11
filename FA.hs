module FA where

import Text.PrettyPrint.Boxes (Box, center1, char, hsep, left, render, text, vcat)
import Data.List (isInfixOf)
import qualified Data.Set as Set (Set, member, findMax, findMin, fromList, singleton, toList)

type State = Int
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
  | Set.findMax (q fa1) >= Set.findMin (q fa2)
              = (fa1, FA q2' (alpha fa2) d2' start2' end2')
  | otherwise = (fa1, fa2)
  where add x s         = Set.fromList $ map (+ x) $ Set.toList s
        q1              = q fa1
        q2              = q fa2
        diff            = Set.findMax q1 + 1 - Set.findMin q2
        q2'             = add diff q2
        d2 state char   = (delta fa2) (state - diff) char
        d2' state char  = add diff (d2 state char)
        start2'         = startState fa2 + diff
        end2'           = add diff (endStates fa2)

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
mydelta state char
  | state == 0 && char == 'a' = Set.singleton 1
  | state == 0 && char == 'b' = Set.singleton 2
  | (state == 1 || state == 2) && char == 'a' = Set.singleton 3
  | (state == 1 || state == 2) && char == 'b' = Set.singleton 4
  | (state == 3 || state == 4) = Set.singleton 5
  | state == 5 = Set.singleton 5

myalphabet = Set.fromList ['a', 'b']

myends = Set.fromList [1, 2, 5]

myq = Set.fromList [0 .. 5]

mybegin = 0

mydfa = FA myq myalphabet mydelta mybegin myends