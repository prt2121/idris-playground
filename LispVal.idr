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
                 | UnboundVar String String
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
    constructor MkIFunc
    fn : L.List LispVal -> Eval LispVal

  public export
  data LispVal = Atom String
               | List (L.List LispVal)
               | DottedList (L.List LispVal) LispVal
               | Number Integer
               | Str String
               | Bool B.Bool
