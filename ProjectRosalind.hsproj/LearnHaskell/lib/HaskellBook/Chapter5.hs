module Chapter5 where

g :: a -> b -> c -> b
g a b c = b

h :: (Num a, Num b) => a -> b -> b
h a b = b

jackal :: (Ord a, Eq b) => a -> b -> a
jackal a b = a

kessel :: (Ord a, Num b) => a -> b -> a
kessel a b = a

prob1 :: a -> a
prob1 a = id a

couple1 :: a -> a -> a
couple1 l r = l

couple2 :: a -> a -> a
couple2 l r = r

third1 :: a -> b -> b
third1 l r = r 

third2 :: a -> b -> b
third1 l r = r