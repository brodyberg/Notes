module Goats where

-- for cardinality, it means that
-- unary constructors are the identity function
data Animals = Animals Int deriving (Eq, Show)

-- tooManyGoats :: Int -> Bool
-- tooManyGoats n = n > 42

newtype Goats = 
  Goats Int deriving (Eq, Show)

newtype Cows = 
  Cows Int deriving (Eq, Show)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

tooManyCows :: Cows -> Bool
tooManyCows (Cows n) = n > 5

cows :: Cows -- Int
cows = Cows 4

goats :: Goats -- Int
-- can't supply Int because the type equation
-- was already satisfied in the newtype so the kind
-- went from * -> * to * there. 
goats = Goats 37

class TooMany a where
  tooMany :: a -> Bool

-- *this* is the instance of TooMany Int
instance TooMany Int where
  tooMany n = n > 42

-- but there *is no* instance of TooMany Double

instance TooMany Goats where
  tooMany (Goats n) = n > 43