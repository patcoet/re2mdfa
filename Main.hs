module Main where

import Data.List.Split
import Data.List (isInfixOf)

import Construct
import Determinize
import FA
import Minimize

-- True if string2 contains none of the characters in string1
none :: String -> String -> Bool
none chars str = not $ or [isInfixOf [c] str | c <- chars]

parse :: String -> Regex a
parse str
  | str == "ε" = Epsilon
  | str == "" = Null
  | none "+*()" str = conscat str
  | isInfixOf "+" str = Plus (parse s1) (parse s2)
  | last str == '*'  = Star (parse (takeWhile (/= '*') str))
    where (s1, s2) = (takeWhile (/= '+') str, drop 1 $ dropWhile (/= '+') str)

main = do putStr "Enter a regular expression: "
          input <- getLine
          putStrLn $ show $ minimize $ determinize $ construct $ parse
            $ filter (/= ' ') input
          main