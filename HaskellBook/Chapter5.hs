module Chapter5 where

g :: a -> b -> c -> b
g a b c = b

h :: (Num a, Num b) => a -> b -> b
h a b = b

jackal :: (Ord a, Eq b) => a -> b -> a
jackal a b = a

kessel :: (Ord a, Num b) => a -> b -> a
kessel a b = a