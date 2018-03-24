module WordNumber where

import Data.List (intercalate)
import Data.Maybe

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

digits :: Int -> [Int]
digits n = 
  if n < 0
  then []
  else 
    n `mod` 10 : 
      case (n `quot` 10) of  
        0 -> []
        x -> digits x

wordNumber :: Int -> String
wordNumber n = 
  intercalate "-" $ catMaybes $ fmap digitToWord $ (reverse . digits) n