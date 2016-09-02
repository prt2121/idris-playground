module Types

mutual

  public export
  data Val = A String -- atom
           | L (List Val) -- list
           | D (List Val) Val -- Dotted List
           | N Integer -- num
           | S String -- string
           | B Bool -- bool
           | PrimitiveFunc (List Val -> ThrowsError Val)
           | Func (List String) (Maybe String) (List Val) Env

  public export
  data Error = ParserE String
             | BadSpecialForm String Val
             | NotFunction String String
             | NumArgs Integer (List Val)
             | TypeMismatch String Val
             | UnboundVar String String

  public export
  ThrowsError : Type -> Type
  ThrowsError = Either Error

  public export
  Env : Type
  Env = List (String, Val)

mutual
  unwordsList : List Val -> String
  unwordsList = unwords . map showVal

  p : Maybe String -> String
  p varargs = case varargs of
                   Nothing => ""
                   Just arg => " . " ++ arg

  export
  showVal : Val -> String
  showVal (A x) = x
  showVal (L xs) = "(" ++ unwordsList xs ++ ")"
  showVal (D xs x) = "(" ++ unwordsList xs ++ " . " ++ showVal x ++ ")"
  showVal (N x) = show x
  showVal (S x) = "\"" ++ x ++ "\""
  showVal (B True) = "#t"
  showVal (B False) = "#f"
  showVal (PrimitiveFunc _) = "<primitive>"
  showVal (Func args varargs body closure) = "(lambda (" ++ (unwords (map show args)) ++ (p varargs) ++ ") ...)"

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
