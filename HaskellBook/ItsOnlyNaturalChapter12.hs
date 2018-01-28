module ItsOnlyNatural where

-- You'll be presented with a datatype to represent
-- the natural numbers. The only values representable
-- with the naturals are whole numbers from zero to
-- infinity. 

-- Your task will be to implement functions to convert
-- Naturals to Integers and Integers to Naturals. 

-- The conversion from Naturals to Integers won't 
-- return Maybe because Integer is a strict superset
-- of Natural. Any Natural can be represented by an
-- Integer, but the same is *not* true of any Integer. 

-- Negative numbers are not valid natural numbers. 

data Nat = 
    Zero 
  | Succ Nat
  deriving (Eq, Show)

-- natToInteger Zero
-- 0

-- natToInteger (Succ Zero)
-- 1

-- natToInteger (Succ (Succ Zero))
-- 2

natToInteger :: Nat -> Integer
natToInteger = undefined

-- integerToNat 0
-- Just Zero

-- integerToNat 1
-- Just (Succ Zero)

-- integerToNat 2
-- Just (Succ (Succ Zero))

-- integerToNat (-1)
-- Nothing

integerToNat :: Integer -> Maybe Nat
integerToNat = undefined

-- Next: SmallLibraryForMaybeChapter12.hs