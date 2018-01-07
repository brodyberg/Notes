module Deconstructing where

data GuessWhat a = 
  Chickenbutt deriving (Eq, Show)

data Id a = 
  MkId a deriving (Eq, Show)

data Product a b = 
  Product a b deriving (Eq, Show)

data Sum a b = 
    First a
  | Second b
  deriving (Eq, Show)

data RecordProduct a b =
  RecordProduct { pFirst :: a
                , pSecond :: b }
                deriving (Eq, Show)

--------------

newtype NumCow = 
  NumCow Int
  deriving (Eq, Show)

newtype NumPig = 
  NumPig Int
  deriving (Eq, Show)

data Farmhouse = 
  Farmhouse NumCow NumPig
  deriving (Eq, Show)

-- Farmhouse' and Farmhouse are the same
type Farmhouse' = Product NumCow NumPig

---------------

newtype NumSheep = 
  NumSheep Int
  deriving (Eq, Show)

data BigFarmhouse = 
  BigFarmhouse NumCow NumPig NumSheep
  deriving (Eq, Show)

type BigFarmHouse' = 
  Product NumCow (Product NumPig NumSheep)

type Name = String
type Age = Int
type LovesMud = Bool

type PoundsOfWool = Int

data CowInfo = 
  CowInfo Name Age
  deriving (Eq, Show)

data PigInfo = 
  PigInfo Name Age LovesMud
  deriving (Eq, Show)

data SheepInfo = 
  SheepInfo Name Age PoundsOfWool
  deriving (Eq, Show)

