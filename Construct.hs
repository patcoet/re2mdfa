module Construct where

import Data.List (isInfixOf)
import qualified Data.Set as Set
  (Set, empty, findMin, findMax, fromList, map, member, singleton, toList, union, unions)

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
  | Set.findMax (q1) >= Set.findMin (q2)
              = (fa1, FA q2' (alpha fa2) d2' start2' end2')
  | otherwise = (fa1, fa2)
  where add x set      = Set.fromList [Set.map (x +) a | a <- Set.toList set]
        q1             = q fa1
        q2             = q fa2
        merge          = Set.unions . Set.toList
        diff           = Set.findMax (merge q1) + 1 - Set.findMin (merge q2)
        q2'            = add diff q2
        d2 state char  = (delta fa2) (merge $ add (-diff) (Set.singleton state)) char
        d2' state char = merge $ add diff (Set.singleton (d2 state char))
        start2'        = merge $ add diff $ Set.singleton (startState fa2)
        end2'          = add diff (endStates fa2)

-- Construct an ε-NFA
construct :: Regex a -> FA
construct Null = construct' q (Set.singleton 'Φ') d 0 (Set.singleton 1)
  where q = Set.fromList [Set.singleton 0, Set.singleton 1]
        d = \x y -> Set.empty
construct Epsilon = construct' q (Set.singleton 'ε') d 0 (Set.singleton 1)
  where q = Set.fromList [Set.singleton 0, Set.singleton 1]
        d state 'ε' = Set.fromList [1 | Set.findMax state == 0]
construct (Atom a) = construct' q (Set.singleton a) d 0 (Set.singleton 1)
  where q = Set.fromList [Set.singleton 0, Set.singleton 1]
        d state char
          | state == Set.singleton 0 && char == a = Set.singleton 1
          | otherwise = Set.empty

construct (Plus r s) = FA q' alpha' delta' (start') (Set.singleton end')
  where (r', s') = rename (construct r, construct s)
        highest x = Set.map (x +) $ Set.findMax $ q s'
        start' = highest 1
        end' = highest 2
        q' = Set.unions [q r', q s', Set.singleton start', Set.singleton end']
        alpha' = Set.unions [alpha r', alpha s', Set.singleton 'ε']
        delta' state char
          | state == start' && char == 'ε'
              = Set.union (startState r') (startState s')
          | Set.member state (Set.union (endStates r') (endStates s'))
              && char == 'ε' = end'
          | Set.member state (q r') = (delta r') state char
          | otherwise = (delta s') state char

construct (Concat r s) = FA q' alpha' delta' (startState r') (endStates s')
  where (r', s') = rename (construct r, construct s)
        q' = Set.union (q r') (q s')
        alpha' = Set.unions [alpha r', alpha s', Set.singleton 'ε']
        delta' state char
          | Set.member state (endStates r') && char == 'ε' = startState s'
          | Set.member state (q r') = (delta r') state char
          | otherwise = (delta s') state char

construct (Star r) = FA q' alpha' delta' start' (Set.singleton end')
  where r' = construct r
        highest x = Set.map (x +) $ Set.findMax $ q r'
        start' = highest 1
        end' = highest 2
        q' = Set.unions [q r', Set.singleton start', Set.singleton end']
        alpha' = Set.union (alpha r') (Set.singleton 'ε')
        delta' state char
          | (Set.member state (endStates r') || state == start')
              && char == 'ε' = Set.union end' (Set.singleton 0)
          | state == end' || char == 'ε' = Set.empty
          | otherwise = (delta r') state char

construct' q alpha delta startState endStates
  = FA q alpha delta (Set.singleton startState) (Set.singleton endStates)