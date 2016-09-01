module MiniParser

import Types

import public Lightyear
import public Lightyear.Char
import public Lightyear.Strings

import Combi

symbol : Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces : Parser ()
spaces = skipMany1 space

parseString : Parser Val
parseString = do
                char '"'
                x <- many $ noneOf "\""
                char '"'
                pure $ S $ pack x

parseAtom : Parser Val
parseAtom = do
              fst <- letter <|> symbol
              rest <- many (alphaNum <|> symbol)
              let atom = pack $ fst :: rest
              pure $ case atom of
                            "#t" => B True
                            "#f" => B False
                            otherwise => A atom

positiveInt : (Num n, Monad m, Stream Char s) => ParserT s m n
positiveInt = do
                ds <- some digit
                let theInt = getInteger ds
                pure $ fromInteger theInt
              where
                getInteger : List (Fin 10) -> Integer
                getInteger = foldl (\a => \b => 10 * a + cast b) 0

parseNumber : Parser Val
parseNumber = pure $ N !positiveInt

mutual
  parseQuoted : Parser Val
  parseQuoted = do
                  char '\''
                  pure $ L [(A "quote"), !parseExpr]

  parseList : Parser Val
  parseList = pure $ L !(sepBy parseExpr MiniParser.spaces)

  parseDottedList : Parser Val
  parseDottedList = do
                      head <- endBy parseExpr MiniParser.spaces
                      tail <- do
                                char '.'
                                MiniParser.spaces
                                parseExpr
                      pure $ D head tail

  export
  parseExpr : Parser Val
  parseExpr = parseAtom
           <|> parseString
           <|> parseNumber
           <|> parseQuoted
           <|> do char '('
                  x <- parseList
                  char ')'
                  pure x
           <|> do char '('
                  x <- parseDottedList
                  char ')'
                  pure x
