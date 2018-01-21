module Phone where

-- 1. Create a datastructure that captures the 
--    phone layout on page 457. 

data Value = 
    One   Char 
  | Two   Char Char 
  | Three Char Char Char
  | Four  Char Char Char Char
  deriving (Eq, Show)

toString :: Value -> String
toString (One c) = [c]
toString (Two c r) = [c, r]
toString (Three c r e) = [c, r, e]
toString (Four c r e w) = [c, r, e, w]

data Key = 
    Num Int
  | NumAlpha Int Value
  | Alpha Int Value
  | Symbol Char Value
  deriving (Eq, Show)

keyToValidValues :: Key -> String
keyToValidValues (Num x)        = show x
keyToValidValues (NumAlpha x v) = show x ++ toString v
keyToValidValues (Alpha x v)    = show x ++ toString v
keyToValidValues (Symbol s v)   = [s] ++ toString v

keyRowToValidValues :: KeyRow -> String
keyRowToValidValues (KeyRow x y z) = 
  foldr (++) "" $ map keyToValidValues [x, y, z]

keyToValidButtons :: Key -> String
keyToValidButtons (Num x)        = show x
keyToValidButtons (NumAlpha x _) = show x
keyToValidButtons (Alpha x _)    = show x
keyToValidButtons (Symbol s _)   = [s]

keyRowToValidButtons :: KeyRow -> String
keyRowToValidButtons (KeyRow x y z) = 
  foldr (++) "" $ map keyToValidButtons [x, y, z]
  
data KeyRow = KeyRow Key Key Key
  deriving (Eq, Show)

data Phone = Phone KeyRow KeyRow KeyRow KeyRow
  deriving (Eq, Show)

class KeyPad a where
  validButtons :: a -> String
  validValues  :: a -> String

instance KeyPad Phone where
  validButtons (Phone kr1 kr2 kr3 kr4) = 
    foldr (++) "" $ map keyRowToValidButtons [kr1, kr2, kr3, kr4]
  validValues (Phone kr1 kr2 kr3 kr4) = 
    foldr (++) "" $ map keyRowToValidValues [kr1, kr2, kr3, kr4]

thisPhone :: Phone
thisPhone = 
  Phone 
    (KeyRow 
      (Num 1)                          (Alpha 2 (Three 'A' 'B' 'C')) (Alpha 3 (Three 'D' 'E' 'F')))
    (KeyRow
      (Alpha 4 (Three 'G' 'H' 'I'))    (Alpha 5 (Three 'J' 'K' 'L')) (Alpha 6 (Three 'M' 'N' 'O')))
    (KeyRow
      (Alpha 7 (Four 'P' 'Q' 'R' 'S')) (Alpha 8 (Three 'T' 'U' 'V')) (Alpha 9 (Four 'W' 'X' 'Y' 'Z')))
    (KeyRow
      (Symbol '*' (One '^'))          (NumAlpha 0 (Two '+' '_'))     (Symbol '#' (Two '.' ',')))

-- 2. Convert the following conversations into the keypresses
--    required to express them. 

convo :: [String]
convo = [
  "Hope is the beginning of despair",
  "Ok Marneas calm down",
  "We were betrayed at Calth",
  "True enough",
  "Only the Emperor can save us now",
  "Since when do Ultramarines say that",
  "Put on your fancy gauntlets and fight"]

-- they hard code valid buttons, but we could
-- derive it from a phone by asking

type Digit = Char
type Presses = Int