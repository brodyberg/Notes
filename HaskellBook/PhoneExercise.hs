module Phone where

data Key = 
    Num Int 
  | NumTwoAlpha   Int Char Char 
  | NumThreeAlpha Int Char Char Char
  | NumFourAlpha  Int Char Char Char Char
  | TwoSymbols    Char Char
  | ThreeSymbols  Char Char Char
  deriving (Eq, Show)

data KeyRow = KeyRow Key Key Key
  deriving (Eq, Show)

data Phone = Phone KeyRow KeyRow KeyRow KeyRow
  deriving (Eq, Show)

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
