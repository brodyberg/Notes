module EqInstances where

data TisAnInteger = 
  TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn x) (TisAn y) = x == y 

data TwoIntegers = 
  Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two x y) (Two x' y') = 
    x == x' &&
    y == y'

data StringOrInt = 
    TisAnInt   Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt x) (TisAnInt y) = x == y
  (==) (TisAString x) (TisAString y) = x == y
  (==) _ _ = False

data Pair a = 
  Pair a a

-- Throws -Wtype-defaults when we pass
-- (Pair 1 2) == (Pair 1 2)
-- Fine when we pass strings
-- (Pair "foo" "bar") == (Pair "foo" "bar")
instance Eq a => Eq (Pair a) where
  (==) (Pair x y) (Pair x' y') =  
    x == x' &&
    y == y'

data Tuple a b = 
  Tuple a b 

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple x y) (Tuple x' y') =  
    x == x' &&
    y == y'
        