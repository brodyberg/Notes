module Zipping where

-- question 1

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (a:as) (b:bs) = (a, b) : zip' as bs

-- question 2

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs

numberLetter n l = l : (show n)

-- *Zipping> zipWith numberLetter [1..26] ['a'..'z']
-- ["a1","b2","c3","d4","e5","f6","g7","h8","i9","j10",
-- "k11","l12","m13","n14","o15","p16","q17","r18","s19",
-- "t20","u21","v22","w23","x24","y25","z26"]

-- question 3

zip'' :: [a] -> [b] -> [(a, b)]
zip'' = zipWith (,)