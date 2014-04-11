module Determinize where

import Control.Monad (filterM)
import qualified Data.Set as Set (delete, singleton)

import FA

-- Turn e-NFA into DFA
-- determinize :: 

-- closure :: State -> Delta -> [State]
-- closure state delta = [state] ++ (delta state 'ε')

-- delta' :: State -> [Char] -> Delta -> [State]
-- delta' state (c:hars) delta
  -- | length (c:hars) == 1 = delta state c
  -- | otherwise = delta state c ++ delta' (concat (delta state c)) hars delta

-- delta' state (c:hars) delta
  -- | delta 


-- delta' :: Delta -> [State] -> Char -> [[State]]
-- delta' delta states char = [delta state char | state <- states]
  
closure :: Delta -> [State] -> [State]
closure delta [] = []
closure delta states = states ++ closure delta (concat [p | p <- delta' 'ε'])
  where delta' char = [delta state char | state <- states]

determinize :: FA -> FA
determinize nfa = FA q' alpha' delta' startState' endStates'
  where pow = filterM (const [True, False]) -- ?????????????
        q' = concat [closure (delta nfa) s | s <- pow (q nfa)]
        alpha' = Set.delete 'ε' (alpha nfa)
        delta' = delta nfa
        startState' = concat $ closure (delta nfa) [startState nfa]
        endStates' = endStates nfa