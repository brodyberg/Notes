module ProjectRosalind.Motif.ExpressionParsing where 

import Text.Parsec (ParseError, SourcePos, getPosition)
import Text.Parsec.String (Parser)
import ProjectRosalind.Motif.Parsec (try)
import ProjectRosalind.Motif.Char (oneOf, noneOf, char, digit, satisfy)
import ProjectRosalind.Motif.Combinator (eof, many1, manyTill, choice, chainl1, count)
import Control.Applicative ((<|>), many)
import Control.Monad (void)
import Data.Char (isLetter, isDigit)
import ProjectRosalind.Motif.FunctionsAndTypesForParsing

numberExamples :: [(String,Integer)]
numberExamples = [("1", 1)
                 ,("23", 23)]
                 
num :: Parser Integer
num = do
    n <- many1 digit
    return (read n)
    
num1 :: Parser Integer
num1 = do
    n <- many digit
    return (read n)
    
varExamples :: [(String,String)]
varExamples = [("test", "test")
              ,("_stuff", "_stuff")
              ,("_1234", "_1234")]
              
var :: Parser String
var = do
    fc <- firstChar
    rest <- many nonFirstChar
    return (fc:rest)
  where
    firstChar = satisfy (\a -> isLetter a || a == '_')
    nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')
    
spanned :: Parser a -> Parser (SourcePos, a)
spanned p = do
  pos <- getPosition
  a <- p
  pure (pos, a)

-- > parseTest (spanned (many1 (char 'a'))) "aaaaafff"
--((line 1, column 1),(line 1, column 6),"aaaaa")

glycosylation :: Parser String-- (SourcePos, SourcePos, String )
glycosylation = do 
  n <- char 'N'
  notP1 <- noneOf ['P']
  sOrt <- oneOf ['S', 'T']    
  notP2 <- noneOf ['P']
  return [n, notP1, sOrt, notP2]

q = regularParse (spanned glycosylation) "QEWRQEWRLLELE"
r = regularParse (spanned glycosylation) "NRTX"


nnn = "MKNKFKTQEELVNHLKTVGFVFANSEIYNGLANAWDYGPLGVLLKNNLKNLWWKEFVTKQ\
\KDVVGLDSAIILNPLVWKASGHLDNFSDPLIDCKNCKARYRADKLIESFDENIHIAENSS\
\NEEFAKVLNDYEISCPTCKQFNWTEIRHFNLMFKTYQGVIEDAKNVVYLRPETAQGIFVN\
\FKNVQRSMRLHLPFGIAQIGKSFRNEITPGNFIFRTREFEQMEIEFFLKEESAYDIFDKY\
\LNQIENWLVSACGLSLNNLRKHEHPKEELSHYSKKTIDFEYNFLHGFSELYGIAYRTNYD\
\LSVHMNLSKKDLTYFDEQTKEKYVPHVIEPSVGVERLLYAILTEATFIEKLENDDERILM\
\DLKYDLAPYKIAVMPLVNKLKDKAEEIYGKILDLNISATFDNSGSIGKRYRRQDAIGTIY\
\CLTIDFDSLDDQQDPSFTIRERNSMAQKRIKLSELPLYLNQKAHEDFQRQCQK"

-- regularParse -- 
xx = manyTill (spanned glycosylation) eof

xxx = count 100 (spanned glycosylation)
xxxx = regularParse xxx nnn

qqqq = regularParse xx nnn
-- manyTill (spanned glycosylation) eof













  
    
