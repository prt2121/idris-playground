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
           | NumArgs Integer (List Val)
           | TypeMismatch String Val

ThrowsError : Type -> Type
ThrowsError = Either Error

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

Show Error where
  show (ParserE e)          = "Parse error " ++ e
  show (BadSpecialForm s v) = s ++ " : " ++ show v
  show (NotFunction s f)    = s ++ " : " ++ show f
  show (NumArgs e f)        = "Expected " ++ show e ++ " args; found " ++ (show $ length f)
  show (TypeMismatch e f)   = "Invalid type: expected " ++ e ++ ", found " ++ show f
  show _                    = "Error!!!"

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

toVect2' : List Val -> Maybe (Vect 2 Val)
toVect2' [x, y] = Just (fromList [x, y])
toVect2' _      = Nothing

toVect2 : List Val -> Either Error (Vect 2 Val)
toVect2 ls = maybeToEither (NumArgs 2 ls) (toVect2' ls)

unpackNum : Val -> Either Error Integer
-- unpackNum (A x)    = ?unpackNum_rhs_1
unpackNum (L [n])  = unpackNum n
-- unpackNum (D xs x) = ?unpackNum_rhs_3
unpackNum (N n)    = Right $ n
unpackNum (S n)    = maybeToEither (TypeMismatch "number" (S n)) $ parseInteger n -- todo
-- unpackNum (B x)    = ?unpackNum_rhs_6
unpackNum notNum   = Left $ TypeMismatch "number" notNum

unpackStr : Val -> Either Error String
unpackStr (S s)      = Right s
unpackStr (N s)      = Right $ show s
unpackStr (B s)      = Right $ show s
unpackStr notString  = Left $ TypeMismatch "string" notString

unpackBool : Val -> Either Error Bool
unpackBool (B b) = Right b
unpackBool notBool  = Left $ TypeMismatch "boolean" notBool

boolBinop : (unpacker : Val -> Either Error a) -> (a -> a -> Bool) -> Vect 2 Val -> Either Error Val
boolBinop unpacker op args = do left <- unpacker $ head args
                                right <- unpacker $ last args
                                pure $ B $ left `op` right

numBoolBinop : (Integer -> Integer -> Bool) -> List Val -> Either Error Val
numBoolBinop op ls = toVect2 ls >>= boolBinop unpackNum op

strBoolBinop : (String -> String -> Bool) -> List Val -> Either Error Val
strBoolBinop op ls = toVect2 ls >>= boolBinop unpackStr op

boolBoolBinop : (Bool -> Bool -> Bool) -> List Val -> Either Error Val
boolBoolBinop op ls = toVect2 ls >>= boolBinop unpackBool op

numericBinop : (Integer -> Integer -> Integer) -> List Val -> Either Error Val
numericBinop op params = (liftA N) $ foldl1 (liftA2 op) $ map unpackNum params

car : List Val -> Either Error Val
car [L (x :: xs)]   = return x
car [D (x :: xs) _] = return x
car [badArg]        = Left $ TypeMismatch "pair" badArg
car badArgList      = Left $ NumArgs 1 badArgList

cdr : List Val -> Either Error Val
cdr [L (x :: xs)]   = return $ L xs
cdr [D [_] x]       = return x
cdr [D (_ :: xs) x] = return $ D xs x
cdr [badArg]        = Left $ TypeMismatch "pair" badArg
cdr badArgList      = Left $ NumArgs 1 badArgList

cons : Vect 2 Val -> Either Error Val
cons [x1, L []]      = return $ L [x1]
cons [x, L xs]       = return $ L $ x :: xs
cons [x, D xs xlast] = return $ D (x :: xs) xlast
cons [x1, x2]        = return $ D [x1] x2

eqv : Vect 2 Val -> Either Error Val
eqv [(B arg1), (B arg2)] = return $ B $ arg1 == arg2
eqv [(N arg1), (N arg2)] = return $ B $ arg1 == arg2
eqv [(S arg1), (S arg2)] = return $ B $ arg1 == arg2
eqv [(A arg1), (A arg2)] = return $ B $ arg1 == arg2
eqv [(D xs x), (D ys y)] = eqv [L $ xs ++ [x], L $ ys ++ [y]]
eqv [(L arg1), (L arg2)] = return $ B $ (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2)
                           where eqvPair (x1, x2) = case eqv [x1, x2] of
                                                         Left err => False
                                                         Right (B v) => v
eqv [_, _]               = return $ B False

-- equal? and Weak Typing: Heterogenous Lists[edit]
-- equal : List Val -> Either Error Val

-- non-lazy version of (&&)
and : Bool -> Bool -> Bool
and True x  = x
and False _ = False

-- non-lazy version of (||)
or : Bool -> Bool -> Bool
or False x = x
or True _  = True

infixr 1 >=>
(>=>) : Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
(>=>) f g = \x => f x >>= g

primitives : List (String, List Val -> Either Error Val)
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop divBigInt),
              ("remainder", numericBinop modBigInt),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop and),
              ("||", boolBoolBinop or),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", toVect2 >=> cons),
              ("eqv?", toVect2 >=> eqv),
              ("eq?", toVect2 >=> eqv)]

applyFunc : (func : String) -> (args : List Val) -> Eff Val [EXCEPTION Error]
applyFunc func args = maybe (raise $ NotFunction "Unrecognized primitive function" func)
                            (\op => case op args of
                                         Right v => pure v
                                         Left e => raise e)
                            $ List.lookup func primitives

eval : Val -> Eff Val [EXCEPTION Error]
eval (L [A "if", pred, conseq, alt]) = do
                                         result <- eval pred
                                         case result of
                                              B False   => eval alt
                                              otherwise => eval conseq
-- eval (A x) = ?eval_rhs_1 -- todo
eval (L [A "quote", val]) = pure val
eval (L (A func :: args)) = do
                              args' <- mapEff eval args
                              applyFunc func args'
                            where
                              mapEff : (eval : Val -> Eff Val [EXCEPTION Error]) -> List Val -> Eff (List Val) [EXCEPTION Error]
                              mapEff eval []        = pure []
                              mapEff eval (x :: xs) = do x' <- eval x
                                                         pure $ x' :: !(mapEff eval xs)
-- eval (D xs x) = ?eval_rhs_3 -- todo
eval val@(N x) = pure $ val
eval val@(S x) = pure $ val
eval val@(B x) = pure $ val
eval badForm   = raise $ BadSpecialForm "Unrecognized special form" badForm

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
