module Phone where

-- 1. Create a datastructure that captures the 
--    phone layout on page 457. 

data Face = 
    Number Int
  | Symbol Char

data Value = 
    Two Char Char
  | Three Char Char Char
  | Four Char Char Char

data Key = 
    Num Int 
  | NumTwoAlpha   Int Char Char 
  | NumThreeAlpha Int Char Char Char
  | NumFourAlpha  Int Char Char Char Char
  | TwoSymbols    Char Char
  | ThreeSymbols  Char Char Char
  deriving (Eq, Show)

keyToValidButtons :: Key -> String
keyToValidButtons (Num x)                   = show x
keyToValidButtons (NumTwoAlpha   x g h)     = show x ++ [g, h]
keyToValidButtons (NumThreeAlpha x g h i)   = show x ++ [g, h, i]
keyToValidButtons (NumFourAlpha  x g h i j) = show x ++ [g, h, i, j]
keyToValidButtons (TwoSymbols      g h)     = [g, h]   
keyToValidButtons (ThreeSymbols    g h i)   = [g, h, i]

keyRowToValidButtons :: KeyRow -> String
keyRowToValidButtons (KeyRow x y z) = 
  foldr (++) "" $ map keyToValidButtons [x, y, z]

data KeyRow = KeyRow Key Key Key
  deriving (Eq, Show)

data Phone = Phone KeyRow KeyRow KeyRow KeyRow
  deriving (Eq, Show)

class KeyPad a where
  validButtons :: a -> String

instance KeyPad Phone where
  validButtons (Phone kr1 kr2 kr3 kr4) = 
    foldr (++) "" $ map keyRowToValidButtons [kr1, kr2, kr3, kr4]

thisPhone :: Phone
thisPhone = 
  Phone 
    (KeyRow 
      (Num 1)                           (NumThreeAlpha 2 'A' 'B' 'C') (NumThreeAlpha 3 'D' 'E' 'F'))
    (KeyRow
      (NumThreeAlpha 4 'G' 'H' 'I')     (NumThreeAlpha 5 'J' 'K' 'L') (NumThreeAlpha 6 'M' 'N' 'O'))
    (KeyRow
      (NumFourAlpha  7 'P' 'Q' 'R' 'S') (NumThreeAlpha 8 'T' 'U' 'V') (NumFourAlpha 9 'W' 'X' 'Y' 'Z'))
    (KeyRow
      (TwoSymbols '*' '^')              (NumTwoAlpha   0 '+' '_')     (ThreeSymbols '#' '.' ','))

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