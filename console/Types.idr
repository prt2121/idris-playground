module Types

public export
data Val = A String -- atom
         | L (List Val) -- list
         | D (List Val) Val -- Dotted List
         | N Integer -- num
         | S String -- string
         | B Bool -- bool

public export
data Error = ParserE String
           | BadSpecialForm String Val
           | NotFunction String String
           | NumArgs Integer (List Val)
           | TypeMismatch String Val
           | UnboundVar String String
