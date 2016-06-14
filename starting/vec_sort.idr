import Data.Vect

insert : Ord elem => (x : elem) -> (sorted : Vect k elem) -> Vect (S k) elem
insert x [] = [x]
insert x (y :: xs) = case x < y of
                        False => y :: insert x xs
                        True => x :: y :: xs

ins_sort : Ord elem => Vect n elem -> Vect n elem
ins_sort [] = []
ins_sort (x :: xs) = let sorted = ins_sort xs in
                      insert x sorted

-- *vec_sort> ins_sort ['C', 'A', 'Z']
-- ['A', 'C', 'Z'] : Vect 3 Char
-- *vec_sort> ins_sort [3,2,9,7,6,5,8]
-- [2, 3, 5, 6, 7, 8, 9] : Vect 7 Integer
