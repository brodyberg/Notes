module PityTheBool where

import Data.Int

-- 1. Given the datatype

-- data BigSmall = 
--     Big Bool
--   | Small Bool
--   deriving (Eq, Show)

-- What is the cardinality of the datatype?
-- True + False | True + False 
-- 2 + 2 = 4

-- 2. Given the datatype

data NumberOrBool = 
    Numba Int8
  | BoolyBool Bool
  deriving (Eq, Show)

-- let myNumba = Numba (-128)
myNumba = Numba (-127)

-- What is the cardinality of NumberOrBool?
-- Int8 + Bool
-- |-127| + 1 + 127 + Bool
-- 255 + 2
-- 257

-- What happens when you create a Numba with a
-- numeric literal larger than 127?
-- *PityTheBool> let myNumba = Numba (-128)

-- <interactive>:74:23: warning: [-Woverflowed-literals]
--     Literal 128 is out of the Int8 range -128..127
--     If you are trying to write a large negative literal, use NegativeLiterals
