module YouCanDoThis where

import Control.Monad (liftM2)
import Text.ParserCombinators.Parsec
import Numeric (readHex)

-- From RWH Chapter 16 "Using Parsec"

p_query :: CharParser () [(String, Maybe String)]
p_query = p_pair `sepBy` char '&'

p_pair :: CharParser () (String, Maybe String)
p_pair = do
    name <- many1 p_char
    value <- optionMaybe (char '=' >> many p_char)
    return (name, value)

-- liftM2 
--    first argument is a function with two arguments
--    and here we use tuple constructor so the types
--    of the left and right can be different (where in 
--    examples of liftM2 which use (+) the types have
--    to agree. So the first argument to (,) is String
--    and the second is Maybe String. We can see this
--    clearly from the type signature of p_pair_app1 
--    anyway. 
--    And the monad wrappers for our arguments are: 
--    String on the left and Maybe on the right?
--    liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
--   

p_pair_app1 :: CharParser () (String, Maybe String)
p_pair_app1 = 
    liftM2 (,) (many1 p_char) (optionMaybe (char '=' >> many p_char))

urlBaseChars = ['a'..'z']++['A'..'Z']++['0'..'9']++"$-_.!*'(),"

-- it'd be interesting to make all this fail with really good 
-- error messages

p_char :: CharParser () Char
p_char = oneOf urlBaseChars
    <|> (char '+' >> return ' ') -- convert + to space
    <|> p_hex                    -- at this point: 
                                 -- not a base-char
                                 -- not a + sign
                                 -- all that's left is % so we go into hex parsing

p_ha :: CharParser () Char
p_ha = do
    a <- hexDigit
    return a

p_hab :: CharParser () (Char, Char)
p_hab = do
    a <- hexDigit
    b <- hexDigit
    return (a,b)
    
p_hex :: CharParser () Char
p_hex = do
    char '%'
    a <- hexDigit
    b <- hexDigit
    let ((d, _):_) = readHex [a,b]
    return . toEnum $ d

hexify :: Char -> Char -> Char
hexify a b = toEnum . fst . head . readHex $ [a,b]

a_hex :: CharParser () Char
a_hex = hexify <$> (char '%' *> hexDigit) <*> hexDigit

-- CharParser: https://hackage.haskell.org/package/parsec-3.1.11/docs/Text-ParserCombinators-Parsec-Char.html
-- CharParser is just GenParser Char st 
-- which is super interesting because it's "filled" type-wise
-- from the left just like currying (but for types)

