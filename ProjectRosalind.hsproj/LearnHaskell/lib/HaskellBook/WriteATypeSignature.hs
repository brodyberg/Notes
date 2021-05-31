module WriteATypeSignature where

-- chapter 5 "write a type signature" pg. 151

functionH :: [a] -> a
functionH (x:_)= x

functionC :: (Ord a) => a -> a -> Bool
functionC x y =
  if (x > y) then True else False

functionS :: (a, b) -> b
functionS (x, y) = y
