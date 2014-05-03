module Construct where

import Data.Set (empty, findMin, findMax, fromList, map, member, singleton,
                toList, union, unions)
import Prelude hiding (map)

import FA

data Regex a = Null
               | Epsilon
               | Atom Char
               | Plus (Regex a) (Regex a)
               | Concat (Regex a) (Regex a)
               | Star (Regex a)

-- Make sure state names don't overlap
rename :: (FA, FA) -> (FA, FA)
rename (fa1, fa2)
  | findMax (q1) >= findMin (q2)
              = (fa1, FA q2' (alpha fa2) d2' start2' end2')
  | otherwise = (fa1, fa2)
  where add x set      = fromList [map (x +) a | a <- toList set]
        q1             = q fa1
        q2             = q fa2
        merge          = unions . toList
        diff           = findMax (merge q1) + 1 - findMin (merge q2)
        q2'            = add diff q2
        d2 state char  = (delta fa2) (merge $ add (-diff) (singleton state)) char
        d2' state char = merge $ add diff (singleton (d2 state char))
        start2'        = merge $ add diff $ singleton (startState fa2)
        end2'          = add diff (endStates fa2)

-- Construct an ε-NFA
construct :: Regex a -> FA
construct Null = construct' q (singleton 'Φ') d 0 (singleton 1)
  where q = fromList [singleton 0, singleton 1]
        d = \x y -> empty
construct Epsilon = construct' q (singleton 'ε') d 0 (singleton 1)
  where q = fromList [singleton 0, singleton 1]
        d state char
          | state == singleton 0 && char == 'ε' = singleton 1
          | otherwise = empty
construct (Atom a) = construct' q (singleton a) d 0 (singleton 1)
  where q = fromList [singleton 0, singleton 1]
        d state char
          | state == singleton 0 && char == a = singleton 1
          | otherwise = empty

construct (Plus r s) = FA q' alpha' delta' start' (singleton end')
  where (r', s') = rename (construct r, construct s)
        highest x = map (x +) $ findMax $ q s'
        start' = highest 1
        end' = highest 2
        q' = unions [q r', q s', singleton start', singleton end']
        alpha' = unions [alpha r', alpha s', singleton 'ε']
        delta' state char
          | state == start' && char == 'ε'
              = union (startState r') (startState s')
          | member state (union (endStates r') (endStates s'))
              && char == 'ε' = end'
          | member state (q r') = (delta r') state char
          | member state (q s') = (delta s') state char
          | otherwise = empty

construct (Concat r s) = FA q' alpha' delta' (startState r') (endStates s')
  where (r', s') = rename (construct r, construct s)
        q' = union (q r') (q s')
        alpha' = unions [alpha r', alpha s', singleton 'ε']
        delta' state char
          | member state (endStates r') && char == 'ε' = startState s'
          | member state (q r') = (delta r') state char
          | otherwise = (delta s') state char

construct (Star r) = FA q' alpha' delta' start' (singleton end')
  where r' = construct r
        highest x = map (x +) $ findMax $ q r'
        start' = highest 1
        end' = highest 2
        q' = unions [q r', singleton start', singleton end']
        alpha' = union (alpha r') (singleton 'ε')
        delta' state char
          | (member state (endStates r') || state == start')
              && char == 'ε' = union end' (startState r')
          | member state (q r') = (delta r') state char
          | otherwise = empty

construct' q alpha delta startState endStates
  = FA q alpha delta (singleton startState) (singleton endStates)

-- Temporary testing helper
conscat :: [Char] -> Regex a
conscat (x:xs)
  | length (x:xs) == 1 = Atom x
  | length xs == 1 = Concat (Atom x) (Atom (head xs))
  | otherwise = Concat (Atom x) (conscat xs)
