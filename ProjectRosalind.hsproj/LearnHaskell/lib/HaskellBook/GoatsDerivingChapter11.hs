{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GoatsDefinitelyDeriving where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = 
  Goats Int deriving (Eq, Show, TooMany)

-- newtype Plane = 
--   F16 String Int deriving (Eq, Show, TooMany)
-- constructor of a newtype must have exactly
-- one field which is what I was wondering wrt
-- generalized newtype deriving in the sense of
-- how do we "just get" to instances of Int in 
-- Goats - because Goats disappears at runtime 
-- and we're left with the Int and we have the 
-- instance due to the pragma
-- and my whole question was if the newtype had
-- two parameters, both of which were instances
-- of TooMany how would "it" choose and the answer
-- is that it wouldn't because that entire 
-- scenar is illegal

goats :: Goats
goats = Goats 12

bgoats :: Goats
bgoats = Goats 88

plane :: Plane
plane = F16 "foob" 5