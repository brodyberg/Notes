module Chapter7GrabBag where

f1 x y z = x * y * z
f2 x y = \z -> x * y * z
f3 x = \y -> \z -> x * y * z
f4 = \x -> \y -> \z -> x * y * z
-- why is f4 specialized to Integer??
f5 = \v -> \x -> \y -> \z -> v * x * y * z
-- yes, f5 is also specialized to Integer
-- is this due to # of params or because
-- the name is bound to an anonymous fn
f0 = \x -> x * 3
-- f0 is also specialized! 
-- so there's something about functions that 
-- take parameters as opposed to symbols which name
-- anonymous functions that causes a difference in 
-- specialization of the types

addOne1 x = x + 1
addOne2 = \x -> x + 1

addOneIfOdd1 n = case odd n of 
  True -> f n
  False -> n
  where f n = n + 1

addOneIfOdd2 n = case odd n of 
  True -> (\n -> n + 1) n
  False -> n

addFive1 x y = (if x > y then y else x) + 5
addFive2 x y f = (f x y) + 5  
-- addFive2 3 4 (\x y -> if x > y then y else x)

mflip1 f = \x -> \y -> f y x
mflip2 f x y = f y x
-- mflip2 (\a b -> a - b) 5 9