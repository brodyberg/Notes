module YouCanDoThis where

import Numeric (readHex)

-- From RWH Chapter 16 "Using Parsec"

p_query :: CharParser () [(String, Maybe String)]
p_query = p_pair `sepBy` char '&'

p_pair :: CharParser () (String, Maybe String)
p_pair = do
    name <- many1 p_char
    value <- optionMaybe (char '=' >> many p_char)
    return (name, value)

urlBaseChars = ['a'..'z']++['A'..'Z']++['0'..'9']++"$-_.!*'(),"

p_char :: CharParser () Char
p_char = oneOf urlBaseChars
    <|> (char '+' >> return ' ')
    <|> p_hex

p_hex :: CharParser () Char
p_hex = do
    char '%'
    a <- hexDigit
    b <- hexDigit
    let ((d, _):_) = readHex [a,b]
    return . toEnum $ d

-- CharParser: https://hackage.haskell.org/package/parsec-3.1.11/docs/Text-ParserCombinators-Parsec-Char.html
-- CharParser is just GenParser Char st 
-- which is super interesting because it's "filled" type-wise
-- from the left just like currying (but for types)