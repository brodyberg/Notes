module Chapter9Exercises where

import Data.Maybe
import Data.Char

-- exercise 2

str = "HbEfLrLxO"

onlyUpperHello = filter isUpper str

-- exercise 3

julie = "julie"

capitalize :: [Char] -> [Char]
capitalize [] = []
capitalize (a:as) = toUpper a : as

-- exercise 4

toAllUpper :: [Char] -> [Char]
toAllUpper [] = []
toAllUpper (a:as) = toUpper a : toAllUpper as

-- exercise 5

capitalizeFirst :: [Char] -> Maybe Char
capitalizeFirst [] = Nothing
capitalizeFirst (a:as) = Just $ toUpper a

capitalizeFirst' :: [Char] -> [Char]
capitalizeFirst' [] = []
capitalizeFirst' (a:as) = [toUpper a]

-- exercise 6

-- as composed
capitalizeFirst'' :: [Char] -> [Char]
capitalizeFirst'' [] = []
capitalizeFirst'' as = [(toUpper . head) as]

-- as composed point-free
-- I can't figure out how to make this point-free
-- *and* total. If I include [] = [] and the 
-- point-free version together (without the case
-- version) I get: 
-- Chapter9.hs:45:1: error:
-- Equations for `capitalizeFirst_' have different numbers of arguments
--   Chapter9.hs:45:1-24
--   Chapter9.hs:46:1-47
-- Failed, modules loaded: none.
-- but if I change it to handle the empty list
-- it's not point-free...
capitalizeFirst_ :: [Char] -> [Char]
-- capitalizeFirst_ [] = []
capitalizeFirst_ xs = 
  case length xs of 
    0 -> []
    _ -> ((\c -> [c]) . toUpper . head) xs


    --capitalizeFirst_ = (\c -> [c]) . toUpper . head