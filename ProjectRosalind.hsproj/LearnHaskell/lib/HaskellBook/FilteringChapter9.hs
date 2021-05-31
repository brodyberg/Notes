module Filtering where

-- problem 1

divisibleByThree :: [Integer] -> [Integer]
divisibleByThree = filter (\x -> (rem x 3) == 0) 

-- problem 2

-- compose this with length to find how many there are

howMany = length $ divisibleByThree [1..30]

howMany' = length . divisibleByThree 

count = howMany' [1..30]

showMany'' = (length . divisibleByThree) [1..30]

-- problem 3
str = "the brown dog was a goof"

disallowed = ["the", "a", "an"]
articles w = not $ elem w disallowed

myFilter :: [Char] -> [[Char]]
-- myFilter xs = filter (\w -> not $ elem w disallowed) $ words xs 
myFilter xs = filter articles $ words xs