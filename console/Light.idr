module Main

import public Lightyear
import public Lightyear.Char
import public Lightyear.Strings

import Combi
import Data.String

import Effects
import Effect.Exception

data Val = A String -- atom
         | L (List Val) -- list
         | D (List Val) Val -- Dotted List
         | N Integer -- num
         | S String -- string
         | B Bool -- bool

data Error = ParserE String
           | BadSpecialForm String Val
           | NotFunction String String

ThrowsError : Type -> Type
ThrowsError = Either Error

Show Error where
  show (ParserE e) = "Parse error " ++ e
  show _           = "Error!!!"

mutual
  unwordsList : List Val -> String
  unwordsList = unwords . map showVal

  showVal : Val -> String
  showVal (A x) = x
  showVal (L xs) = "(" ++ unwordsList xs ++ ")"
  showVal (D xs x) = "(" ++ unwordsList xs ++ " . " ++ showVal x ++ ")"
  showVal (N x) = show x
  showVal (S x) = "\"" ++ x ++ "\""
  showVal (B True) = "#t"
  showVal (B False) = "#f"

Show Val where
  show = showVal

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

readExpr : String -> Eff Val [EXCEPTION Error]
readExpr str = case parse parseExpr str of
                   Left err => raise $ ParserE err
                   Right v  => pure v

unpackNum : Val -> Integer
-- unpackNum (A x)    = ?unpackNum_rhs_1
unpackNum (L [n])  = unpackNum n
-- unpackNum (D xs x) = ?unpackNum_rhs_3
unpackNum (N n)    = n
unpackNum (S n)    = fromMaybe 0 $ parseInteger n -- todo
-- unpackNum (B x)    = ?unpackNum_rhs_6
unpackNum _        = 0

numericBinop : (Integer -> Integer -> Integer) -> List Val -> Val
numericBinop op params = N $ foldl1 op $ map unpackNum params

primitives : List (String, List Val -> Val)
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop divBigInt),
              ("remainder", numericBinop modBigInt)]

apply' : (func : String) -> (args : List Val) -> Val
apply' func args = maybe (B False) (\op => op args) $ lookup func primitives

eval : Val -> Eff Val [EXCEPTION Error]
eval (A x) = ?eval_rhs_1 -- todo
eval (L [A "quote", val]) = pure val
eval (L (A func :: args)) = do
                              args' <- mapEff eval args
                              pure $ apply' func args'
                            where
                              mapEff : (eval : Val -> Eff Val [EXCEPTION Error]) -> List Val -> Eff (List Val) [EXCEPTION Error]
                              mapEff eval []        = pure []
                              mapEff eval (x :: xs) = do x' <- eval x
                                                         pure $ x' :: !(mapEff eval xs)
eval (D xs x) = ?eval_rhs_3 -- todo
eval val@(N x) = pure $ val
eval val@(S x) = pure $ val
eval val@(B x) = pure $ val
eval badForm = raise $ BadSpecialForm "Unrecognized special form" badForm


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

evaluate : String -> Eff Val [EXCEPTION Error]
evaluate expr = eval !(readExpr expr)

main : IO ()
main = do
          (_ :: expr :: _) <- getArgs
          case the (Either Error Val) $ run $ evaluate expr of
               Left e => putStrLn $ show e
               Right v => putStrLn $ show v
