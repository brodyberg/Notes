{-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE MagicHash #-}

module BrodyPrelude where

import qualified GHC.Types (Bool(True, False))
import Data.Char -- as Prelude.Data.Char
import GHC.Char (eqChar)
-- import GHC.Prim (eqChar#)
--import GHC.Prim (eqChar#)

type String = [Char]

data Bool = True | False 

not :: Bool -> Bool
not True = False
not False = True

class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool 

class Show a where
  show :: a -> String
  
instance Show Bool where
  show True = "True"
  show False = "False"

instance Eq Char where
  x == y = case (x `eqChar` y) of 
    GHC.Types.True -> True
    GHC.Types.False -> False
  x /= y = not (x == y)

instance Show [Char] where
  show s = s

-- instance Show String where
--   show s = s