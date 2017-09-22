module Prettify where

import SimpleJSON
import Numeric (showHex)
import Data.Bits (shiftR)
import Data.Char (ord)

data Doc = ToBeDefined
         deriving (Show)

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

text :: String -> Doc
text str = undefined

double :: Double -> Doc
double num = undefined

enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right

(<>) :: Doc -> Doc -> Doc
a <> b = undefined

char :: Char -> Doc
char c = undefined

hcat :: [Doc] -> Doc
hcat xs = undefined

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
              Just r -> text r
              Nothing | mustEscape c -> hexEscape c
                      | otherwise    -> char c
    where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where ch a b = (a, ['\\',b])

smallHex :: Int -> Doc
smallHex x  = text "\\u"
           <> text (replicate (4 - length h) '0')
           <> text h
    where h = showHex x ""

astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
    where a = (n `shiftR` 10) .&. 0x3ff
          b = n .&. 0x3ff 
            -- .&. is bitwise AND
            -- https://hackage.haskell.org/package/base-4.10.0.0/docs/Data-Bits.html#v:.-38-.

hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise   = astral (d - 0x10000)
  where d = ord c