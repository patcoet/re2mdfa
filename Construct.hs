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
        d2 state char  = (delta fa2) (merge $ add diff (Set.singleton state)) char
        d2' state char = merge $ add (-diff) (Set.singleton (d2 state char))
        start2'        = merge $ add diff $ Set.singleton (startState fa2)
        end2'          = add diff (endStates fa2)

-- Constructs a nondeterministic finite automaton with epsilon transitions
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

-- construct (Plus r s) = construct' q' alpha' delta' (highest+1) (Set.singleton (highest+2))
  -- where (consr, conss) = rename (construct r, construct s)
        -- highest = Set.findMax (Set.union (Set.findMax $ q consr) (Set.findMax $ q conss))
        -- q' = Set.unions [Set.findMax $ q consr, Set.findMax $ q conss, Set.singleton (highest+1), Set.singleton (highest+2)]
        -- alpha' = Set.union (alpha consr) (alpha conss)
        -- delta' state symbol
          -- | state == Set.singleton (highest+1) && symbol == 'ε'
              -- = Set.fromList [startState consr, startState conss]
          -- | Set.member state (Set.union (endStates consr) (endStates conss))
              -- && symbol == 'ε' = Set.singleton $ Set.singleton (highest + 2)
          -- | Set.member state (q consr) = (delta consr) state symbol
          -- | otherwise = (delta conss) state symbol

-- construct (Concat r s)
  -- = construct' q' alpha' delta' (startState' consr) (endStates' conss)
  -- where (consr, conss) = rename (construct r, construct s)
        -- q' = Set.union (Set.findMax $ q consr) (Set.findMax $ q conss)
        -- alpha' = Set.unions [alpha consr, alpha conss, Set.singleton 'ε']
        -- delta' state symbol
          -- | Set.member state (endStates consr) && symbol == 'ε' = Set.singleton $ startState conss
          -- | Set.member state (q consr) = (delta consr) state symbol
          -- | otherwise = (delta conss) state symbol
        -- startState' = Set.findMax . startState
        -- endStates' = Set.unions . Set.toList . endStates

-- construct (Star r) = construct' q' alpha' delta' (highest+1) (Set.singleton (highest+2))
  -- where consr = construct r
        -- highest = Set.findMax (Set.findMax $ q consr)
        -- q' = Set.unions [Set.findMax $ q consr, Set.singleton (highest+1), Set.singleton (highest+2)]
        -- alpha' = Set.union (alpha consr) (Set.singleton 'ε')
        -- delta' state symbol
          -- | (Set.member state (endStates consr) || state == Set.singleton (highest+1))
              -- && symbol == 'ε' = Set.singleton $ Set.fromList [highest+2, 0]
          -- | state == Set.singleton (highest+2) || symbol == 'ε' = Set.singleton $ Set.empty
          -- | otherwise = (delta consr) state symbol

construct' q alpha delta startState endStates
  = FA q alpha delta (Set.singleton startState) (Set.singleton endStates)