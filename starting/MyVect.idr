module MyVect

import Data.Vect

-- The take function, on List, has type:
take' : Nat -> List a -> List a
take' Z [] = []
take' (S k) (x :: xs) = x :: take' k xs

take'' : (n : Nat) -> Vect (n + m) a -> Vect n a
take'' Z _ = []
take'' (S k) (x :: xs) = x :: take'' k xs

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

tryIndex : Integer -> Vect n a -> Maybe a
tryIndex {n} x xs = case integerToFin x n of
                        Nothing => Nothing
                        (Just x) => Just (index x xs)

-- *MyVect> MyVect.tryIndex 3 [1,2,3,4,5]
-- Just 4 : Maybe Integer
