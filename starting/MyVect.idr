module MyVect

import Data.Vect

append : Vect n elem -> Vect m elem -> Vect (n + m) elem
append [] ys = ys
append (x :: xs) ys = x :: append xs ys

zip' : Vect n a -> Vect n b -> Vect n (a, b)
zip' [] _ = []
zip' (x :: xs) (y :: ys) = (x, y) :: zip' xs ys

-- Fin = finitely bounded
index'' : Fin n -> Vect n a -> a
index'' FZ (x :: xs) = x
index'' (FS x) (y :: xs) = index'' x xs

-- *MyVect> MyVect.index'' 3 [1,2,3,4,5]
-- 4 : Integer
