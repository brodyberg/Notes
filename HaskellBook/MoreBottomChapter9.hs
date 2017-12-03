module MoreBottom where

import Data.Bool

itIsMystery :: [Char] -> [Bool]
itIsMystery xs = map (\x -> elem x "aeiou") xs


reverser :: [Integer] -> [Integer]
reverser = map (\x -> if x == 3 then (-x) else x)

-- More Bottom Question 6

-- From Chapter7Exercises.hs
-- foldBool' :: (Ord a) => a -> a -> Bool -> a 
-- foldBool' x y b
--   | b = x
--   | otherwise = y

reverser' :: [Integer] -> [Integer]
reverser' = map (\x -> bool x (-x) (x == 3))