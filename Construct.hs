module Construct where

import Data.List (isInfixOf)
import qualified Data.Set as Set (Set, singleton, union)

import FA

data Regex a = Null
               | Epsilon
               | Atom a
               | Plus (Regex a) (Regex a)
               | Concat (Regex a) (Regex a)
               | Star (Regex a)

-- Constructs a nondeterministic finite automaton with epsilon transitions
construct :: Regex a -> FA
construct Null = FA ["0000", "0001"] (Set.singleton 'Φ') d "0000" ["0001"]
  where d _ _ = []
construct Epsilon = FA ["0000","0001"] (Set.singleton 'ε') (\x y -> ["0001"]) "0000" ["0001"]
  where d state 'ε' = ["0001" | state == "0000"]
construct (Atom a) = FA ["0000","0001"] (Set.singleton 'a') d "0000" ["0001"]
  where d state a = ["0001" | state == "0000"]

construct (Plus r s) = FA q' alpha' delta' "q-" ["q+"]
  where (consr, conss) = rename (construct r, construct s)
        q' = q consr ++ q conss
        alpha' = Set.union (alpha consr) (alpha conss)
        delta' state symbol
          | state == "q-" && symbol == 'ε'
              = [startState consr ++ startState conss]
          | isInfixOf [state] (endStates consr ++ endStates conss)
              && symbol == 'ε' = ["q+"]
          | isInfixOf [state] (q consr) = (delta consr) state symbol
          | otherwise = (delta conss) state symbol

construct (Concat r s)
  = FA q' alpha' delta' (startState consr) (endStates conss)
  where (consr, conss) = rename (construct r, construct s)
        q' = q consr ++ q conss
        alpha' = Set.union (Set.union (alpha consr) (alpha conss)) (Set.singleton 'e')
        delta' state symbol
          | isInfixOf [state] (endStates consr) && symbol == 'e' = ["0002"]
          | isInfixOf [state] (q consr) = (delta consr) state symbol
          | otherwise = (delta conss) state symbol

construct (Star r) = FA q' alpha' delta' "q-" ["q+"]
  where consr = construct r
        q' = q consr
        alpha' = Set.union (Set.singleton 'ε') (alpha consr)
        delta' state symbol
          | (isInfixOf [state] (endStates consr) || state == "q-")
              && symbol == 'ε' = ["q+", "q0"]
          | state == "q+" = ["q+"]
          | otherwise = (delta consr) state symbol