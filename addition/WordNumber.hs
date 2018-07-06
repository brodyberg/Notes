module WordNumber where

import Data.List (intercalate)
import Data.Maybe

wordToDigit :: String -> Maybe Int
wordToDigit s = 
  case s of 
    "zero" -> Just 0 
    "one"  -> Just 1
    "two"  -> Just 2
    "three"-> Just 3
    "four" -> Just 4
    "five" -> Just 5
    "six"  -> Just 6
    "seven"-> Just 7
    "eight"-> Just 8
    "nine" -> Just 9
    _      -> Nothing

digitToWord :: Int -> Maybe String
digitToWord n = 
  case n of
    0 -> Just "zero"
    1 -> Just "one"
    2 -> Just "two"
    3 -> Just "three"
    4 -> Just "four"
    5 -> Just "five"
    6 -> Just "six"
    7 -> Just "seven"
    8 -> Just "eight"
    9 -> Just "nine"
    _ -> Nothing

-- words :: String -> [String]
-- words s = 

digits :: Int -> [Int]
digits n = 
  if n < 0
  then []
  else 
    n `mod` 10 : 
      case (n `quot` 10) of  
        0 -> []
        x -> digits x

numbersToBase10 :: [Int] -> Int
numbersToBase10 []     = 0
numbersToBase10 (n:ns) = 
  n * (10 ^ (length ns)) + numbersToBase10 ns

numberWord :: String -> Int
numberWord s =
  numbersToBase10 $ catMaybes $ fmap wordToDigit $ words s
      
wordNumber :: Int -> String
wordNumber n = 
  intercalate " " $ catMaybes $ fmap digitToWord $ (reverse . digits) n