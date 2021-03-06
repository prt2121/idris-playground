module Eval

import Effects
import Effect.Exception
import Effect.State
import Effect.StdIO

import Prelude.List as L
import Prelude.Bool as B

import Parser
import LispVal
import Utils

export
Prim : Type
Prim = List (String, LispVal)

Unary : Type
Unary = LispVal -> Eval LispVal

Binary : Type
Binary = LispVal -> LispVal -> Eval LispVal


binop : Binary -> List LispVal -> Eval LispVal
binop op args = case args of
                     [x, y] => op x y
                     _ => raise $ NumArgs 2 args


numOp : (Integer -> Integer -> Integer) -> LispVal -> LispVal -> Eval LispVal
numOp op (Number x) (Number y) = pure $ Number $ op x  y
numOp op (Number x) y          = raise $ TypeMismatch "Number" y
numOp op x          _          = raise $ TypeMismatch "Number" x


numBoolBinop : (Integer -> Integer -> B.Bool) -> LispVal -> LispVal -> Eval LispVal
numBoolBinop op (Number x) (Number y) = pure $ Bool $ op x  y
numBoolBinop op (Number x) y          = raise $ TypeMismatch "bool" y
numBoolBinop op x          _          = raise $ TypeMismatch "bool" x


-- todo: more ops
export
primEnv : Prim
primEnv = [ ("+", Fun $ MkFun $ binop $ numOp (+))
            , ("-", Fun $ MkFun $ binop $ numOp (-))
            , ("*", Fun $ MkFun $ binop $ numOp (*))
            , ("/", Fun $ MkFun $ binop $ numOp div)
            , ("mod", Fun $ MkFun $ binop $ numOp mod)
            , ("quotient", Fun $ MkFun $ binop $ numOp divBigInt)
            , ("remainder", Fun $ MkFun $ binop $ numOp modBigInt)
            , ("=", Fun $ MkFun $ binop $ numBoolBinop (==))
            , ("<", Fun $ MkFun $ binop $ numBoolBinop (<))
            , (">", Fun $ MkFun $ binop $ numBoolBinop (>))
            , ("/=", Fun $ MkFun $ binop $ numBoolBinop (/=))
            , (">=", Fun $ MkFun $ binop $ numBoolBinop (>=))
            , ("<=", Fun $ MkFun $ binop $ numBoolBinop (<=))]


mutate : String -> LispVal -> (String, LispVal) -> (String, LispVal)
mutate atom exp (a, b) = if a == atom
                         then (a, exp)
                         else (a, b)


addToEnv : String -> LispVal -> EnvCtx -> EnvCtx
addToEnv n v ls = (n, v) :: ls


mutual
  eval : LispVal -> Eval LispVal
  eval val@(Number n)  = pure val
  eval val@(Str s)     = pure val
  eval val@(Bool b)    = pure val
  eval (List Nil)      = pure Nil
  eval Nil             = pure Nil
  eval val@(Atom atom) = getVar val
  eval (List [Atom "quote", val]) = pure val
  eval (List [Atom "if", pred, conseq, alt]) = case !(eval pred) of
                                                    Bool False => eval alt
                                                    _  => eval conseq
  eval (List [Atom "def", (Atom val), exp]) = defineVar (Atom val) exp
  eval (List [Atom "define", (Atom val), exp]) = defineVar (Atom val) exp
  eval (List [Atom "lambda", params, exp]) = do p <- evalToList params
                                                pure $ Lambda (MkFun (\args => (evalArgsExpEnv args p exp))) !get
  eval (List [(Atom fn), a, b]) =
    do
      func <- getVar $ Atom fn
      case func of
           (Fun (MkFun f)) => f [!(eval a), !(eval b)]
           (Lambda (MkFun f) boundEnv) => do put boundEnv
                                             f [!(eval a), !(eval b)]
           _ => raise $ NotFunction "Unrecognized primitive function" $ show func
  eval somethingElse = raise $ Default $ "unmatched case " ++ show somethingElse


  evalArgsExpEnv : List LispVal -> List LispVal -> LispVal -> Eval LispVal
  evalArgsExpEnv args params exp =
    do bindVars $ zipWith MkPair params args
       eval exp


  setLocal : String -> LispVal -> EnvCtx -> Eval LispVal
  setLocal atom exp env = do update (addToEnv atom exp)
                             eval exp

  setVar : LispVal -> LispVal -> Eval LispVal
  setVar n@(Atom atom) exp = do env <- get
                                case lookup atom env of
                                     Just x => setLocal atom exp env
                                     Nothing => raise $ UnboundVar "unbound variable" n
  setVar _  exp = raise $ LispErr "variables can only be assigned to atoms"


  defineVar : LispVal -> LispVal -> Eval LispVal
  defineVar (Atom atom) exp = setLocal atom exp !get
  defineVar _  exp = raise $ LispErr "can only bind to Atom type valaues"


  getVar : LispVal -> Eval LispVal
  getVar n@(Atom atom) = case lookup atom !get of
                              Nothing => raise $ UnboundVar "unbound variable" n
                              Just val => pure val
  getVar _             = raise $ LispErr "variables can only be assigned to atoms"


  evalToList : LispVal -> Eval (L.List LispVal)
  evalToList expr =
    do
      val <- eval expr
      case val of
           List x => pure x
           _      => raise $ Default "error evaluating into list"


  cons : L.List LispVal -> Eval LispVal
  cons [x, y@(List _)] =
    do
      v <- eval x
      v' <- evalToList y
      pure $ List $ v :: v'
  cons [c] =
    do
      _ <- eval c
      pure $ List [c]
  cons []  = pure $ List []

  -- traverse
  traverse' : L.List LispVal -> Eff (List LispVal) [STATE EnvCtx, EXCEPTION LispError]
  traverse' [x]       = pure [x]
  traverse' (x :: xs) = do x' <- eval x
                           pure $ x' :: !(traverse' xs)
  traverse' []        = raise $ Default "empty body"


  uncurryDef : (LispVal, LispVal) -> Eval LispVal
  uncurryDef = uncurry defineVar


  bindVars : L.List (LispVal, LispVal) -> Eval ()
  bindVars [] = pure ()
  bindVars (x :: xs) =
    do uncurryDef x
       bindVars xs


-- end mutual

runParse_ : String -> Either LispError LispVal
runParse_ = mapLeft (\_ => Default "parser error") . readExpr


testEnv : EnvCtx
testEnv = ("x", Number 42) :: primEnv


textToEval : String -> Eval LispVal
textToEval input =
  case runParse_ input of
       Right val => eval val
       Left err  => raise $ err


runApp : EnvCtx -> Eval b -> Eff (EnvCtx, b) [STATE EnvCtx, STDIO, EXCEPTION LispError]
runApp code action =
  do
    put code
    v <- action
    pure (!get, v)


-- fix me
export
evalExpr : EnvCtx -> String -> IO EnvCtx
evalExpr env expr =
  do
    case the (Either LispError LispVal) $ runInit (env :: () :: []) $ textToEval expr of
      Left err => do putStrLn $ show err
                     pure $ env
      Right val => do (e, v) <- run $ runApp env $ textToEval expr
                      putStrLn $ show v
                      pure $ e
