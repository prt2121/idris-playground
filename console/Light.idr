module Light

import public Lightyear
import public Lightyear.Char
import public Lightyear.Strings

import Combi
import Data.String

import Effects
import Effect.Exception
import Effect.State

export
data Val = A String -- atom
         | L (List Val) -- list
         | D (List Val) Val -- Dotted List
         | N Integer -- num
         | S String -- string
         | B Bool -- bool

export
data Error = ParserE String
           | BadSpecialForm String Val
           | NotFunction String String
           | NumArgs Integer (List Val)
           | TypeMismatch String Val
           | UnboundVar String String

ThrowsError : Type -> Type
ThrowsError = Either Error

Env : Type
Env = List (String, Val)

isBound : String -> Eff Bool [STATE Env]
isBound var = case (lookup var !get) of
                   Nothing => return False
                   Just _  => return True

getVar : String -> Eff Val [STATE Env, EXCEPTION Error]
getVar var = case lookup var !get of
                  Nothing => raise $ UnboundVar "Getting an unbound variable" var
                  Just val => return val

-- update var `n` with v
updateEnv : String -> Val -> List (String, Val) -> List (String, Val)
updateEnv n v ls = map set ls
                   where
                    set : (String, Val) -> (String, Val)
                    set (a, b) = if a == n
                                 then (a, v)
                                 else (a, b)

setVar : String -> Val -> Eff () [STATE Env, EXCEPTION Error]
setVar var val = case lookup var !get of
                      Nothing => raise $ UnboundVar "Getting an unbound variable" var
                      Just v => update (updateEnv var v)

mutual
  unwordsList : List Val -> String
  unwordsList = unwords . map showVal

  export
  showVal : Val -> String
  showVal (A x) = x
  showVal (L xs) = "(" ++ unwordsList xs ++ ")"
  showVal (D xs x) = "(" ++ unwordsList xs ++ " . " ++ showVal x ++ ")"
  showVal (N x) = show x
  showVal (S x) = "\"" ++ x ++ "\""
  showVal (B True) = "#t"
  showVal (B False) = "#f"

export
Show Val where
  show = showVal

export
Show Error where
  show (ParserE e)          = "Parse error " ++ e
  show (UnboundVar m v)     = m ++ ": " ++ v
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
  parseList = return $ L !(sepBy parseExpr Light.spaces)

  parseDottedList : Parser Val
  parseDottedList = do
                      head <- endBy parseExpr Light.spaces
                      tail <- do
                                char '.'
                                Light.spaces
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

toVect2 : List Val -> ThrowsError (Vect 2 Val)
toVect2 ls = maybeToEither (NumArgs 2 ls) (toVect2' ls)

unpackNum : Val -> ThrowsError Integer
-- unpackNum (A x)    = ?unpackNum_rhs_1
unpackNum (L [n])  = unpackNum n
-- unpackNum (D xs x) = ?unpackNum_rhs_3
unpackNum (N n)    = Right $ n
unpackNum (S n)    = maybeToEither (TypeMismatch "number" (S n)) $ parseInteger n -- todo
-- unpackNum (B x)    = ?unpackNum_rhs_6
unpackNum notNum   = Left $ TypeMismatch "number" notNum

unpackStr : Val -> ThrowsError String
unpackStr (S s)      = Right s
unpackStr (N s)      = Right $ show s
unpackStr (B s)      = Right $ show s
unpackStr notString  = Left $ TypeMismatch "string" notString

unpackBool : Val -> ThrowsError Bool
unpackBool (B b) = Right b
unpackBool notBool  = Left $ TypeMismatch "boolean" notBool

boolBinop : (unpacker : Val -> ThrowsError a) -> (a -> a -> Bool) -> Vect 2 Val -> ThrowsError Val
boolBinop unpacker op args = do left <- unpacker $ head args
                                right <- unpacker $ last args
                                pure $ B $ left `op` right

numBoolBinop : (Integer -> Integer -> Bool) -> List Val -> ThrowsError Val
numBoolBinop op ls = toVect2 ls >>= boolBinop unpackNum op

strBoolBinop : (String -> String -> Bool) -> List Val -> ThrowsError Val
strBoolBinop op ls = toVect2 ls >>= boolBinop unpackStr op

boolBoolBinop : (Bool -> Bool -> Bool) -> List Val -> ThrowsError Val
boolBoolBinop op ls = toVect2 ls >>= boolBinop unpackBool op

numericBinop : (Integer -> Integer -> Integer) -> List Val -> ThrowsError Val
numericBinop op params = (liftA N) $ foldl1 (liftA2 op) $ map unpackNum params

car : List Val -> ThrowsError Val
car [L (x :: xs)]   = return x
car [D (x :: xs) _] = return x
car [badArg]        = Left $ TypeMismatch "pair" badArg
car badArgList      = Left $ NumArgs 1 badArgList

cdr : List Val -> ThrowsError Val
cdr [L (x :: xs)]   = return $ L xs
cdr [D [_] x]       = return x
cdr [D (_ :: xs) x] = return $ D xs x
cdr [badArg]        = Left $ TypeMismatch "pair" badArg
cdr badArgList      = Left $ NumArgs 1 badArgList

cons : Vect 2 Val -> ThrowsError Val
cons [x1, L []]      = return $ L [x1]
cons [x, L xs]       = return $ L $ x :: xs
cons [x, D xs xlast] = return $ D (x :: xs) xlast
cons [x1, x2]        = return $ D [x1] x2

eqv : Vect 2 Val -> ThrowsError Val
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
-- equal : List Val -> ThrowsError Val

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

primitives : List (String, List Val -> ThrowsError Val)
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

export
evaluate : String -> Eff Val [EXCEPTION Error]
evaluate expr = eval !(readExpr expr)
