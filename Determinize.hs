module Determinize where

import Control.Monad (filterM)
import Data.Set (Set, delete, difference, empty, filter, fromList, insert,
                intersection, map, singleton, toList, union, unions)
import Prelude hiding (filter, map)

import FA

-- Compute the ε-closure of a state
closure :: Delta -> State -> State
closure delta state
  | state == empty = empty
  | otherwise = unions $
    [state] ++ [closure delta (singleton s) | s <- toList (delta state 'ε')]

-- ε-NFA -> DFA
determinize :: FA -> FA
determinize (FA q alpha delta start ends) = FA q' alpha' delta'' start' ends'
  where q' = insert (singleton (-1)) $ map (closure delta) q
        alpha' = delete 'Φ' $ delete 'ε' alpha
        start' = closure delta start
        ends' = fromList [s | s <- toList q', end <- toList ends,
          intersection s end /= empty]
        delta' state char = closure delta
          (unions [delta (singleton s) char | s <- toList state])
        delta'' state char
          | delta' state char == empty = singleton (-1)
          | otherwise = delta' state char