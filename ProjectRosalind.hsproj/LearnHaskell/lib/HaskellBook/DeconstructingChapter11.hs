module Deconstructing where

data GuessWhat = 
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

data Animal = 
    Cow CowInfo
  | Pig PigInfo
  | Sheep SheepInfo
  deriving (Eq, Show)

-- or alternatively
type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)

-- bess' is a direct use of the
-- CowInfo data constructor

-- bess is a call to the Sum First data 
-- constructor and we're specifying the type
-- as animal' and that matches because First
-- of Animal' must be CowInfo

-- elmer works because the Second Second of 
-- Animal needs to be a SheepInfo

-- elmo doesn't work because Animal' First
-- is required to be a CowInfo 

-- *Deconstructing> let sheep = SheepInfo "Baaaa" 5 5
-- sheep :: SheepInfo
-- *Deconstructing> :t First (Second sheep)
-- First (Second sheep) :: Sum (Sum a SheepInfo) b

trivialValue :: GuessWhat
trivialValue = Chickenbutt

idInt :: Id Integer
idInt = MkId 10

type Awesome = Bool

person :: Product Name Awesome
person = Product "Simon" True

data Twitter = 
  Twitter deriving (Eq, Show)

data AskFm = 
  AskFm deriving (Eq, Show)

socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter

-- *Deconstructing> type SN = Sum Twitter AskFm
-- type SN = Sum Twitter AskFm
-- *Deconstructing> Second Twitter :: SN

-- <interactive>:55:1: error:
--     * Couldn't match type `Twitter' with `AskFm'
--       Expected type: SN
--         Actual type: Sum Twitter Twitter
--     * In the expression: Second Twitter :: SN
--       In an equation for `it': it = Second Twitter :: SN

data SocialNetwork =
    Twitter
  | AskFm
  deriving (Eq, Show)