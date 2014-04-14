module Determinize where

import Control.Monad (filterM)
import qualified Data.Set as Set
  (Set, delete, difference, empty, filter, findMin, fromList, intersection, map, singleton, toList, union, unions)

import FA

closure :: Delta -> State -> Set.Set State
closure delta state = closure' delta (Set.singleton state)

closure' :: Delta -> Set.Set State -> Set.Set State
closure' delta states
  | states == Set.empty = states
  | otherwise = Set.union states $
    closure' delta $ Set.difference (Set.fromList [p | p <- delta' 'ε']) states
  where delta' char = Set.toList $ Set.map (\x -> delta x char) states

-- Turn ε-NFA into DFA
determinize :: FA -> FA
determinize (FA q alpha delta start ends) = FA q' alpha' delta' start' ends'
  where pow = filterM (const [True, False]) . Set.toList -- ????
        q' = Set.map Set.unions $ Set.map Set.toList $ Set.fromList [Set.filter (/= Set.empty) $ closure delta (head s) | s <- pow q, s /= []]
        alpha' = Set.delete 'ε' (alpha)
        delta' state char = Set.unions $ Set.toList $ closure delta
          (Set.unions [delta (Set.singleton s) char | s <- Set.toList state])
        start' = Set.unions $ Set.toList $ closure delta start
        ends' = Set.fromList
          [s | s <- Set.toList q', end <- Set.toList ends,
          Set.intersection s end /= Set.empty]