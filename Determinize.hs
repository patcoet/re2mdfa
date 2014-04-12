module Determinize where

import Control.Monad (filterM)
import qualified Data.Set as Set
  (Set, delete, empty, findMin, fromList, map, singleton, toList, union, unions)

import FA

closure :: Delta -> State -> Set.Set State
closure delta state = closure' delta (Set.singleton state)

closure' :: Delta -> Set.Set State -> Set.Set State
closure' delta states
  | states == Set.empty = states
  | otherwise = Set.union states $
    closure' delta (Set.fromList [p | p <- Set.toList $ delta' 'ε'])
  where delta' char = Set.unions
          $ Set.toList $ Set.map (\x -> delta x char) states

-- Turn ε-NFA into DFA
determinize :: FA -> FA
determinize (FA q alpha delta start ends)
  = FA q' alpha' delta start endStates'
  where pow = filterM (const [True, False]) . Set.toList -- ????
        -- q' = Set.fromList [closure (delta) (Set.fromList s) | s <- pow (q)]
        q' = Set.unions [closure delta (head s) | s <- pow q]
        alpha' = Set.delete 'ε' (alpha)
        -- delta' state char = closure (delta nfa) $ Set.fromList [Set.findMin $ (delta nfa) state char]
        -- startState' = Set.findMin $ closure (delta nfa) (Set.singleton (startState nfa))
        endStates' = ends