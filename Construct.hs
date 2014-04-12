module Construct where

import Data.List (isInfixOf)
import qualified Data.Set as Set
  (Set, empty, findMax, fromList, member, singleton, toList, union, unions)

import FA

data Regex a = Null
               | Epsilon
               | Atom Char
               | Plus (Regex a) (Regex a)
               | Concat (Regex a) (Regex a)
               | Star (Regex a)

-- Constructs a nondeterministic finite automaton with epsilon transitions
construct :: Regex a -> FA
construct Null = construct' (Set.fromList [0, 1]) (Set.singleton 'Φ') d 0 (Set.singleton 1)
  where d = \x y -> Set.empty
construct Epsilon = construct' (Set.fromList [0, 1]) (Set.singleton 'ε') d 0 (Set.singleton 1)
  where d state 'ε' = Set.singleton $ Set.fromList [1 | Set.findMax state == 0]
construct (Atom a) = construct' (Set.fromList [0, 1]) (Set.singleton a) d 0 (Set.singleton 1)
  where d state a = Set.singleton $ Set.fromList [1 | Set.findMax state == 0]

construct (Plus r s) = construct' q' alpha' delta' (highest+1) (Set.singleton (highest+2))
  where (consr, conss) = rename (construct r, construct s)
        highest = Set.findMax (Set.union (Set.findMax $ q consr) (Set.findMax $ q conss))
        q' = Set.unions [Set.findMax $ q consr, Set.findMax $ q conss, Set.singleton (highest+1), Set.singleton (highest+2)]
        alpha' = Set.union (alpha consr) (alpha conss)
        delta' state symbol
          | state == Set.singleton (highest+1) && symbol == 'ε'
              = Set.fromList [startState consr, startState conss]
          | Set.member state (Set.union (endStates consr) (endStates conss))
              && symbol == 'ε' = Set.singleton $ Set.singleton (highest + 2)
          | Set.member state (q consr) = (delta consr) state symbol
          | otherwise = (delta conss) state symbol

construct (Concat r s)
  = construct' q' alpha' delta' (startState' consr) (endStates' conss)
  where (consr, conss) = rename (construct r, construct s)
        q' = Set.union (Set.findMax $ q consr) (Set.findMax $ q conss)
        alpha' = Set.unions [alpha consr, alpha conss, Set.singleton 'ε']
        delta' state symbol
          | Set.member state (endStates consr) && symbol == 'ε' = Set.singleton $ startState conss
          | Set.member state (q consr) = (delta consr) state symbol
          | otherwise = (delta conss) state symbol
        startState' = Set.findMax . startState
        endStates' = Set.unions . Set.toList . endStates

construct (Star r) = construct' q' alpha' delta' (highest+1) (Set.singleton (highest+2))
  where consr = construct r
        highest = Set.findMax (Set.findMax $ q consr)
        q' = Set.unions [Set.findMax $ q consr, Set.singleton (highest+1), Set.singleton (highest+2)]
        alpha' = Set.union (alpha consr) (Set.singleton 'ε')
        delta' state symbol
          | (Set.member state (endStates consr) || state == Set.singleton (highest+1))
              && symbol == 'ε' = Set.singleton $ Set.fromList [highest+2, 0]
          | state == Set.singleton (highest+2) || symbol == 'ε' = Set.singleton $ Set.empty
          | otherwise = (delta consr) state symbol

construct' q alpha delta startState endStates
  = FA (Set.singleton q) alpha delta (Set.singleton startState) (Set.singleton endStates)