module ParsecArticle where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.Parsec
import Text.Parsec.Combinator
import Data.List
import Data.Char
import Debug.Trace


-- import Text.Parsec.String (Parser)
-- import Text.Parsec.String.Char (anyChar)
-- import Text.Parsec.String.Char
-- import FunctionsAndTypesForParsing (regularParse, parseWithEof, parseWithLeftOver)
-- import Data.Char
-- import Text.Parsec.String.Combinator (many1)


-- parseBibtex :: String -> Either ParseError [[(String, String)]]
parseBibtex :: String -> Either ParseError [[String]]
parseBibtex input = parse readBibtex "(unknown)" input


readBibtex = endBy readBibtexEntry (char '\n')
  where readBibtexEntry = do
          char '@'
          a <- many1 (noneOf "{")
          c <- between (char '{') (char '}') (sepBy parseContent (char '\n'))
          return c
        parseContent = do
          -- key <- many1 (noneOf " =")
          -- many (char ' ')
          -- char '='
          -- many (char ' ')
          value <- many (noneOf "\n")
          -- char '\n'
          -- return (key, value)
          return value
          
          
-- readBibtex = many1 readBibtexEntry
--   where readBibtexEntry = do
--           many1 (noneOf "{")
--           between (char '{') (char '}') parseContent
--         parseContent = do
--           nick <- endBy (many1 (noneOf ",")) (char ',')
--           sepBy field (string "\n")
--         field = do
--           key <- many1 (noneOf " =")
--           char '='
--           value <- many1 (noneOf ",\n")
--           return (key, value)




