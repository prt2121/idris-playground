module LispVal

import Prelude.Bool as B
import Prelude.List as L

import Effects
import Effect.Exception
import Effect.State

mutual

  public export
  data LispError = NumArgs Integer (L.List LispVal)
                 | TypeMismatch String LispVal
                 | BadSpecialForm String LispVal
                 | NotFunction String String
                 | UnboundVar String LispVal
                 | Default String
                 | LispErr String

  public export
  EnvCtx : Type
  EnvCtx = L.List (String, LispVal)

  public export
  Eval : Type -> Type
  Eval a = Eff a [STATE EnvCtx, EXCEPTION LispError]

  public export
  record IFunc where
    constructor MkFun
    fn : L.List LispVal -> Eval LispVal

  public export
  data LispVal = Atom String
               | List (L.List LispVal)
               | DottedList (L.List LispVal) LispVal
               | Number Integer
               | Str String
               | Fun IFunc
               | Lambda IFunc EnvCtx
               | Bool B.Bool

mutual
  unwordsList : List LispVal -> String
  unwordsList = unwords . map showVal

  p : Maybe String -> String
  p varargs = case varargs of
                  Nothing => ""
                  Just arg => " . " ++ arg

  export
  showVal : LispVal -> String
  showVal (Atom x) = x
  showVal (List xs) = "(" ++ unwordsList xs ++ ")"
  showVal (DottedList xs x) = "(" ++ unwordsList xs ++ " . " ++ showVal x ++ ")"
  showVal (Number x) = show x
  showVal (Str x) = "\"" ++ x ++ "\""
  showVal (Bool True) = "#t"
  showVal (Bool False) = "#f"
  showVal (Fun _) = "internal function"
  showVal (Lambda _ _) = "lambda function"

export
Show LispVal where
     show = showVal

export
Show LispError where
     show (UnboundVar m v)     = m ++ ": " ++ show v
     show (BadSpecialForm s v) = s ++ " : " ++ show v
     show (NotFunction s f)    = s ++ " : " ++ show f
     show (NumArgs e f)        = "Expected " ++ show e ++ " args; found " ++ (show $ length f)
     show (TypeMismatch e f)   = "Invalid type: expected " ++ e ++ ", found " ++ show f
     show (Default s)          = s
     show (LispErr s)          = s
     show _                    = "Error!!!"
