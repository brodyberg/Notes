module ProjectRosalind.Motif.FunctionsAndTypesForParsing where

import Text.Parsec (ParseError)
import Text.Parsec.String (Parser)
import ProjectRosalind.Motif.Parsec (parse)
import ProjectRosalind.Motif.Char (oneOf)
import ProjectRosalind.Motif.Combinator (eof,manyTill,anyToken)
import Control.Applicative ((<$>), (<*>), (<*), (*>), many)
import Control.Monad (void)

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""

parseWithLeftOver :: Parser a -> String -> Either ParseError (a,String)
parseWithLeftOver p = parse ((,) <$> p <*> leftOver) ""
  where leftOver = manyTill anyToken eof
  
parseWithWSEof :: Parser a -> String -> Either ParseError a
parseWithWSEof p = parseWithEof (whiteSpace *> p)
  where whiteSpace = void $ many $ oneOf " \n\t"