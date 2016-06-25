import Data.Vect

append : Vect n elem -> Vect m elem -> Vect (n + m) elem
append [] ys = ys
append (x :: xs) ys = x :: append xs ys

zip : Vect n a -> Vect n b -> Vect n (a, b)
zip [] _ = []
zip (x :: xs) (y :: ys) = (x, y) :: zip xs ys
