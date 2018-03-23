module Chapter6Exercises where

import Data.List (sort)

data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- printPerson (Person False)

data Mood = Blah
          | Woot deriving (Show, Eq)

settleDown :: Mood -> Mood
settleDown x = if x == Woot
                 then Blah
                 else x
    
-- settleDown Blah
-- settleDown Woot

type Subject = String
type Verb = String
type Object = String

data Sentence = 
  Sentence Subject Verb Object 
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

data Rocks = 
  Rocks String deriving (Eq, Show)

data Yeah = 
  Yeah Bool deriving (Eq, Show)

data Papu = 
  Papu Rocks Yeah
  deriving (Eq, Show)

equalityForAll :: Papu -> Papu -> Bool 
equalityForAll p p' = p == p'

-- no instance of Ord
-- comparePapus :: Papu -> Papu -> Bool 
-- comparePapus p p' = p > p

-- Chapter 6 "Match the types"
-- 1 
-- -- i :: Num a => a 
-- i :: a
-- --i = 1
-- i = 'c'

--f :: Float 
--f :: Num a => a 
f :: Fractional a => a
f = 1.0

--f' :: Float
f' :: RealFrac a => a 
f' = 1.0

-- freud :: a -> a 
freud :: Ord a => a -> a
freud x = x 

--freud' :: a -> a
freud' :: Int -> Int
freud' x = x

myX = 1 :: Int
sigmund :: Int -> Int -- fine
-- sigmund :: a -> a  -- not fine
sigmund x = myX

myX' = 1 :: Int
sigmund' :: Int -> Int -- fine
-- sigmund' :: Num a => a -> a -- not fine 
sigmund' x = myX' 

-- jung :: Ord a => [a] -> a
jung :: [Int] -> Int
jung xs = head (sort xs)

-- young :: [Char] -> Char
young :: Ord a => [a] -> a 
young xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char -- fine
-- signifier :: Ord a => [a] -> a -- not fine
signifier xs = head (mySort xs)

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f x y = f x == y -- Eq to what?

arith :: Num b
      => (a -> b)
      -> Integer
      -> a 
      -> b
arith f x y = f y
















