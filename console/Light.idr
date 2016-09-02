module Light

import public Lightyear

import Data.String

import Effects
import Effect.Exception
import Effect.State

import MiniParser
import Types

isBound : String -> Eff Bool [STATE Env]
isBound var = case (lookup var !get) of
                   Nothing => pure False
                   Just _  => pure True

getVar : String -> Eff Val [STATE Env, EXCEPTION Error]
getVar var = case lookup var !get of
                  Nothing => raise $ UnboundVar "unbound variable" var
                  Just val => pure val

-- update var `n` with v
updateList : String -> Val -> List (String, Val) -> List (String, Val)
updateList n v ls = map set ls
                   where
                    set : (String, Val) -> (String, Val)
                    set (a, b) = if a == n
                                 then (a, v)
                                 else (a, b)

addToList : String -> Val -> List (String, Val) -> List (String, Val)
addToList n v ls = (n, v) :: ls

setVar : String -> Val -> Eff Val [STATE Env, EXCEPTION Error]
setVar var val = case lookup var !get of
                      Nothing => raise $ UnboundVar "unbound variable" var
                      Just v => do update (updateList var v)
                                   pure val

defineVar : String -> Val -> Eff Val [STATE Env, EXCEPTION Error]
defineVar var val = do case !(isBound var) of
                            True => pure !(setVar var val)
                            False => do update (addToList var val)
                                        pure val

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
car [L (x :: xs)]   = pure x
car [D (x :: xs) _] = pure x
car [badArg]        = Left $ TypeMismatch "pair" badArg
car badArgList      = Left $ NumArgs 1 badArgList

cdr : List Val -> ThrowsError Val
cdr [L (x :: xs)]   = pure $ L xs
cdr [D [_] x]       = pure x
cdr [D (_ :: xs) x] = pure $ D xs x
cdr [badArg]        = Left $ TypeMismatch "pair" badArg
cdr badArgList      = Left $ NumArgs 1 badArgList

cons : Vect 2 Val -> ThrowsError Val
cons [x1, L []]      = pure $ L [x1]
cons [x, L xs]       = pure $ L $ x :: xs
cons [x, D xs xlast] = pure $ D (x :: xs) xlast
cons [x1, x2]        = pure $ D [x1] x2

eqv : Vect 2 Val -> ThrowsError Val
eqv [(B arg1), (B arg2)] = pure $ B $ arg1 == arg2
eqv [(N arg1), (N arg2)] = pure $ B $ arg1 == arg2
eqv [(S arg1), (S arg2)] = pure $ B $ arg1 == arg2
eqv [(A arg1), (A arg2)] = pure $ B $ arg1 == arg2
eqv [(D xs x), (D ys y)] = eqv [L $ xs ++ [x], L $ ys ++ [y]]
eqv [(L arg1), (L arg2)] = pure $ B $ (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2)
                           where eqvPair (x1, x2) = case eqv [x1, x2] of
                                                         Left err => False
                                                         Right (B v) => v
eqv [_, _]               = pure $ B False

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

-- bindVars : List (String, Val) -> Eff Env [STATE Env]
-- bindVars bindings = do update (++ bindings)
--                        pure !get

-- bindVars : List (String, Val) -> Eff Env [STATE Env]

bindVars : List (String, Val)
           -> List (String, Val)
           -> Eff Env [STATE Env, EXCEPTION Error]
bindVars env bindings = do put (env ++ bindings)
                           pure !get

bindVarArgs : (varargs : Maybe String)
              -> (env : Env)
              -> Eff Env [STATE Env, EXCEPTION Error]

evalBody : (env : Env) -> Val

apply : Val -> (args : List Val) -> Eff Val [STATE Env, EXCEPTION Error]
apply (PrimitiveFunc func) args = case func args of
                                       Right v => pure v
                                       Left e => raise e
apply (Func params varargs body closure) args =
  if length params /= length args && varargs == Nothing
     then raise $ NumArgs (cast $ length params) args
     else do env <- (bindVars closure (zip params args))
             env' <- bindVarArgs varargs env
             pure $ evalBody env'

applyFunc : (func : String) -> (args : List Val) -> Eff Val [EXCEPTION Error]
applyFunc func args = maybe (raise $ NotFunction "Unrecognized primitive function" func)
                            (\op => case op args of
                                         Right v => pure v
                                         Left e => raise e)
                            $ List.lookup func primitives


eval' : Val -> Eff Val [STATE Env, EXCEPTION Error]
eval' val@(S x) = pure $ val
eval' val@(N x) = pure $ val
eval' val@(B x) = pure $ val
eval' (A id)    = getVar id
eval' (L [A "if", pred, conseq, alt]) = do result <- eval' pred
                                           case result of
                                                B False   => eval' alt
                                                otherwise => eval' conseq
eval' (L [A "set!", A var, form]) = setVar var !(eval' form)
eval' (L [A "define", A var, form]) = defineVar var !(eval' form)
eval' (L (A func :: args)) = do args' <- mapEff eval' args
                                applyFunc func args'
                             where mapEff : (f : Val -> Eff Val [STATE Env, EXCEPTION Error]) -> List Val -> Eff (List Val) [STATE Env, EXCEPTION Error]
                                   mapEff f []        = pure []
                                   mapEff f (x :: xs) = do x' <- f x
                                                           pure $ x' :: !(mapEff f xs)
eval' badForm = raise $ BadSpecialForm "Unrecognized special form" badForm


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

export
evaluate : String -> Eff Val [STATE Env, EXCEPTION Error]
evaluate expr = eval' !(readExpr expr)
