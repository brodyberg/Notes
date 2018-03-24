module WordNumber where

import Data.List (intersperse)

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

digitsBORKED :: Int -> [Int]
digitsBORKED n = reverse $ go n
--digits n = go n
  where 
    go 0 = []
    go x = (x `mod` 10) : digitsBORKED (x `quot` 10)
-- ex. digits 542
--    >[4,5,2]
-- ex. digits 5421
--    >[2,5,4,1]

-- *WordNumber> reverse $ (542 `mod` 10) : ((542 `quot` 10) `mod` 10) : (((542 `quot` 10) `quot` 10) `mod` 10) : []
-- [5,4,2]
-- *WordNumber> reverse $ (5421 `mod` 10) : ((5421 `quot` 10) `mod` 10) : (((5421 `quot` 10) `quot` 10) `mod` 10) : ((((5421 `quot` 10) `quot` 10) `quot` 10) `mod` 10) : []
-- [5,4,2,1]

-- goal: (recursive) thing that without reverse will make 5421 into 1245
theDigits' :: Int -> [Int]
theDigits' 0 = []
theDigits' n = n `mod` 10 : theDigits' (n `quot` 10)

theDigits :: Int -> [Int]
theDigits n = 
  n `mod` 10 : 
    case (n `quot` 10) of  
      0 -> []
      x -> theDigits x

-- theDigits 0 -> []

-- fmap digitToWord $ theDigits 1234

-- the conclusion is: we aren't descending in quot quot quot quot 
-- mod manner somehow, figure that out. s

-- wordNumber :: Int -> String
-- wordNumber n = undefined
--  fmap digitToWord $ digits n