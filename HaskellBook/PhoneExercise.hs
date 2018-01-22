module Phone where

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Char (isUpper, toLower)

-- 1. Create a datastructure that captures the 
--    phone layout on page 457. 

data Phone = Phone String [String]
  deriving (Eq, Show)

standardPhone :: Phone
standardPhone
       = Phone "123456789*0#"
               ["",     "abc", "def", 
                "ghi",  "jkl", "mno",
                "pqrs", "tuv", "wxyz", 
                "^",    "+ ",  ".,"]

standardCharToTaps :: Char 
                   -> [(Digit, Presses)]
standardCharToTaps = charToTaps standardPhone

charToTaps :: Phone
  -> Char 
  -> [(Digit, Presses)]
charToTaps (Phone keys values) c = 
  if isUpper c 
  then ('*', 1) : (charToTaps (Phone keys values) $ toLower c)
  else thePress
  where
    f (k, v) acc = 
      if elem c (v ++ [k]) 
      then (k, pressCount c v): acc 
      else acc
    pressCount :: Char -> String -> Presses
    pressCount k v = (fromJust $ elemIndex c (v ++ [k])) + 1
    thePress = foldr f [] $ zip keys values
    
(Phone keys values) = standardPhone
kv = zip keys values

convo :: [String]
convo = [
  "Hope is the beginning of despair",
  "Ok Marneas calm down",
  "We were betrayed at Calth",
  "True enough",
  "Only the Emperor can save us now",
  "Since when do Ultramarines say that",
  "Put on your fancy gauntlets and fight"]

type Digit = Char
type Presses = Int

-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]

cellPhonesDead :: Phone
               -> String
               -> [(Digit, Presses)]
cellPhonesDead = undefined