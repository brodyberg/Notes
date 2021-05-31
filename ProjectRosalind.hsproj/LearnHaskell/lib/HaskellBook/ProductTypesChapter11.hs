module ProductTypes where

data QuantumBool = 
    QuantumTrue
  | QuantumFalse
  | QuantumBoth
  deriving (Eq, Show)

data TwoQs = 
  MkTwoQs QuantumBool QuantumBool
  deriving (Eq, Show)

-- data Person = 
--   MkPerson String Int
--   deriving (Eq, Show)

-- jm = MkPerson "julie" 108
-- ca = MkPerson "chris" 16

-- namae :: Person -> String
-- namae (MkPerson s _) = s

data Person = 
  Person { name :: String
         , age :: Int }
         deriving (Eq, Show)

-- name and age are functions! 
-- :t name
-- :t age