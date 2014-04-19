module Determinize where

import Control.Monad (filterM)
import Data.Set (Set, delete, difference, empty, filter, fromList, intersection,
                map, singleton, toList, union, unions)
import Prelude hiding (filter, map)

import FA

-- Compute the ε-closure of a state
closure :: Delta -> State -> Set State
closure delta state = closure' delta (singleton state)

closure' :: Delta -> Set State -> Set State
closure' delta states
  | states == empty = states
  | otherwise = union states $
    closure' delta $ difference (fromList [p | p <- delta']) states
  where delta' = toList $ map (\x -> delta x 'ε') states

-- Turn ε-NFA into DFA
determinize :: FA -> FA
determinize (FA q alpha delta start ends) = FA q' alpha' delta'' start' ends'
  where pow = filterM (const [True, False]) . toList -- ????
        q' = map unions $ map toList $ fromList 
          ([filter (/= empty) $ closure delta (head s)
          | s <- pow q, s /= []] ++ [singleton $ singleton $ -1])
        alpha' = delete 'ε' (alpha)
        delta' state char = unions $ toList $ closure delta
          (unions [delta (singleton s) char | s <- toList state])
        delta'' state char
          | delta' state char == empty = singleton (-1)
          | otherwise = delta' state char
        start' = unions $ toList $ closure delta start
        ends' = fromList
          [s | s <- toList q', end <- toList ends,
          intersection s end /= empty]
