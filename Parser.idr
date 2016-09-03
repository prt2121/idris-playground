module Parser

import LispVal

import Lightyear
import Lightyear.Char
import Lightyear.Strings as L

import Combinator

export
data ParseError = ParseErr String

symbol : Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces : Parser ()
spaces = skipMany1 space

parseString : Parser LispVal
parseString = do
                char '"'
                x <- many $ noneOf "\""
                char '"'
                pure $ Str $ pack x

parseAtom : Parser LispVal
parseAtom = do
              fst <- letter <|> symbol
              rest <- many (alphaNum <|> symbol)
              let atom = pack $ fst :: rest
              pure $ case atom of
                            "#t" => Bool True
                            "#f" => Bool False
                            otherwise => Atom atom

positiveInt : (Num n, Monad m, Stream Char s) => ParserT s m n
positiveInt = do
                ds <- some digit
                let theInt = getInteger ds
                pure $ fromInteger theInt
              where
                getInteger : List (Fin 10) -> Integer
                getInteger = foldl (\a => \b => 10 * a + cast b) 0

parseNumber : Parser LispVal
parseNumber = pure $ Number !positiveInt

mutual
  parseQuoted : Parser LispVal
  parseQuoted = do
                  char '\''
                  pure $ List [(Atom "quote"), !parseExpr]

  parseList : Parser LispVal
  parseList = pure $ List !(sepBy parseExpr Parser.spaces)

  parseDottedList : Parser LispVal
  parseDottedList = do
                      head <- endBy parseExpr Parser.spaces
                      tail <- do
                                char '.'
                                Parser.spaces
                                parseExpr
                      pure $ DottedList head tail

  parseExpr : Parser LispVal
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

-- parse and mapLeft ParseErr
export
readExpr : String -> Either ParseError LispVal
readExpr = mirror . (map ParseErr) . mirror . (parse parseExpr)
