module GoatsNotDeriving where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = 
  Goats Int deriving (Eq, Show)

instance TooMany Goats where
  tooMany (Goats n) = tooMany n 

goats :: Goats
goats = Goats 12

bgoats :: Goats
bgoats = Goats 88