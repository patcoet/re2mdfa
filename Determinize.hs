module Determinize where

import Data.Set (delete, empty, fromList, insert, intersection, map, singleton,
                toList, unions)

import FA

-- Compute the ε-closure of a state
closure :: Delta -> State -> State
closure delta state
  | state == empty = empty
  | otherwise = unions $
    [state] ++ [closure delta (singleton s) | s <- toList (delta state 'ε')]

closure' delta state =
  unions $ toList $ Data.Set.map (\x -> closure delta (singleton x)) state

-- ε-NFA -> DFA
determinize :: FA -> FA
determinize (FA q alpha delta start ends) = FA q' alpha' delta'' start' ends'
  where q' = insert (singleton (-1)) $ fromList 
          [closure delta s | s <- Prelude.map (unions) $ powerset (toList q)]
        alpha' = delete 'Φ' $ delete 'ε' alpha
        start' = closure delta start
        ends' = fromList [s | s <- toList q', end <- toList ends,
          intersection s end /= empty]
        delta' state char = closure' delta
          (unions [delta (singleton s) char | s <- toList state])
        delta'' state char
          | delta' state char == empty = singleton (-1)
          | otherwise = delta' state char

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = xss /\/ Prelude.map (x:) xss
  where xss = powerset xs

(/\/) :: [a] -> [a] -> [a]
[] /\/ ys = ys
(x:xs) /\/ ys = x : (ys /\/ xs)