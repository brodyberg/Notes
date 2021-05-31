module Vehicles where

data Price = 
  Price Integer deriving (Eq, Show)

data Manufacturer = 
    Mini
  | Mazda
  | Tata
    deriving (Eq, Show)

data Airline = 
    PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
    deriving (Eq, Show)

data Vehicle = 
    Car Manufacturer Price
  | Plane Airline
    deriving (Eq, Show)

myCar :: Vehicle 
myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

-- Exercises Vehicles Chapter 11

-- 1. What is the type of myCar?
-- myCar :: Vehicle

-- 2. Given the following, define the functions: 

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _) = True
isPlane _         = False
--isPlane v = not isCar v

areCars :: [Vehicle] -> Bool
-- areCars vs = all (\v -> isCar v) vs
areCars = all isCar

-- 3. Tell us the manufacturer of a piece of data

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m

-- 4. Given that we're returning the Manufacturer, 
-- what will happen if we use this on a plane?
-- *Vehicles> getManu doge
-- *** Exception: VehiclesChapter11.hs:52:1-21: Non-exhaustive patterns in function getManu

-- 5. Add size as an argument to the Plance constructor

data Vehicle' = 
    Car' Manufacturer Price
  | Plane' Airline Int
    deriving (Eq, Show)

doge' = Plane' PapuAir 4