import Data.Vect

-- using implicit arguments in pattern matching
length : Vect n elem -> Nat
length {n} xs = n

-- give explicit values for implicit arguments by
-- using the notation {n = value}
create_empties : Vect n (Vect 0 a)
create_empties {n = Z} = []
create_empties {n = (S k)} = [] :: create_empties
