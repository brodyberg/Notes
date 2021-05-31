module DogTypes where

data PugType = PugData
data HuskyType a = HuskyData
data DogueDeBordeaux doge = DogueDeBordeaux doge

-- Can't do this
-- a isn't in scope as specified by the left
-- side of the = sign
-- data Foo = Bar a

myPug = PugData :: PugType
myPug' = PugData -- we don't have to be explicit

myHusky :: HuskyType a
myHusky = HuskyData
-- a remains polymorphic

myOtherHusky :: Num a => HuskyType a
myOtherHusky = HuskyData
-- a gets scoped to Num a 

myOtherOtherHusky :: HuskyType [[[Int]]]
myOtherOtherHusky = HuskyData

myDoge :: DogueDeBordeaux Int
myDoge = DogueDeBordeaux 10

-- String doesn't agree with 10
-- myDoge' :: DogueDeBordeaux String
-- myDoge' = DogueDeBordeaux 10

data Doggies a = 
    Husky a 
  | Mastiff a 
  deriving (Eq, Show)

-- Chapter 11 Excercises: Dog Types

-- 1. Is Doggies a type or data constructor?
-- type

-- 2. What is the kind of Doggies?
-- Doggies :: * -> *

-- 3. What is the kind of Doggies String?
-- Doggies String :: *

-- 4. What is the type of Husky 10?
-- Husky 10 :: Num a => Doggies a

-- 5. What is the type of Husky (10 :: Integer)?
-- Husky (10 :: Integer) :: Doggies Integer

-- 6. What is the type of Mastiff "Scooby Doo"?
-- Mastiff "Scooby Doo" :: Doggies [Char]

-- 7. Is DogueDeBordeaux a type or data constructor?
-- both

-- 8. What is the type of DogueDeBordeaux?
-- My answer: a -> DogueDeBordeaux a
-- Real answer:
-- DogueDeBordeaux :: doge -> DogueDeBordeaux doge

-- 9. What is the type of DogueDeBordeaux "doggie"?
-- My answer: DogueDeBordeaux [Char] :: DogueDeBordeaux [Char]
-- Real answer: (My answer mixes term and type-level)
-- DogueDeBordeaux "doggie" :: DogueDeBordeaux [Char]
