module ArtfulDodgy where

dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Integer -> Integer
oneIsOne = dodgy 1
oneIsTwo :: Integer -> Integer
oneIsTwo = (flip dodgy) 2