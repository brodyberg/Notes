module WordNumber where

import Data.List (intercalate)
import Data.Maybe

digitToWord :: Int -> Maybe String
digitToWord n = 
  if n < 0 || n > 9 
  then Nothing
  else Just (translate n)
  where 
    translate :: Int -> String
    translate x = 
      case x of
      0 -> "zero"
      1 -> "one"
      2 -> "two"
      3 -> "three"
      4 -> "four"
      5 -> "five"
      6 -> "six"
      7 -> "seven"
      8 -> "eight"
      9 -> "nine"

-- *WordNumber> reverse $ (542 `mod` 10) : ((542 `quot` 10) `mod` 10) : (((542 `quot` 10) `quot` 10) `mod` 10) : []
-- [5,4,2]
-- *WordNumber> reverse $ (5421 `mod` 10) : ((5421 `quot` 10) `mod` 10) : (((5421 `quot` 10) `quot` 10) `mod` 10) : ((((5421 `quot` 10) `quot` 10) `quot` 10) `mod` 10) : []
-- [5,4,2,1]

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