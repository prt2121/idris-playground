module Main

import public Lightyear
import public Lightyear.Char
import public Lightyear.Strings

import Combi

data Val = A String -- atom
         | L (List Val) -- list
         | D (List Val) Val -- Dotted List
         | N Integer -- num
         | S String -- string
         | B Bool -- bool

symbol : Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces : Parser ()
spaces = skipMany1 space

parseString : Parser Val
parseString = do
                char '"'
                x <- many $ noneOf "\""
                char '"'
                return $ S $ pack x

parseAtom : Parser Val
parseAtom = do
              fst <- letter <|> symbol
              rest <- many (alphaNum <|> symbol)
              let atom = pack $ fst :: rest
              return $ case atom of
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
parseNumber = return $ N !positiveInt

mutual
  parseQuoted : Parser Val
  parseQuoted = do
                  char '\''
                  return $ L [(A "quote"), !parseExpr]

  parseList : Parser Val
  parseList = return $ L !(sepBy parseExpr Main.spaces)

  parseDottedList : Parser Val
  parseDottedList = do
                      head <- endBy parseExpr Main.spaces
                      tail <- do
                                char '.'
                                Main.spaces
                                parseExpr
                      return $ D head tail

  parseExpr : Parser Val
  parseExpr = parseAtom
           <|> parseString
           <|> parseNumber
           <|> parseQuoted
           <|> do char '('
                  x <- parseList
                  char ')'
                  return x
           <|> do char '('
                  x <- parseDottedList
                  char ')'
                  return x

readExpr : String -> String
readExpr str = case parse parseExpr str of
                   Left err => "No match: " ++ show err
                   Right v  => "Found value"

hex : Parser Int
hex = do
  c <- map (ord . toUpper) $ satisfy isHexDigit
  pure $ if c >= ord '0' && c <= ord '9' then c - ord '0'
                                         else 10 + c - ord 'A'

hexQuad : Parser Int
hexQuad = do
  a <- hex
  b <- hex
  c <- hex
  d <- hex
  pure $ a * 4096 + b * 256 + c * 16 + d

main : IO ()
main = do
       (_ :: expr :: _) <- getArgs
       putStrLn (readExpr expr)
