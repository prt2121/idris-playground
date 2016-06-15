import Data.Vect

-- Reimplement transpose_mat using zipWith instead of transpose_helper.

create_empties : Vect n (Vect 0 elem)
create_empties = replicate _ []

transpose_helper : (x : Vect n elem) ->
                   (xs_trans : Vect n (Vect k elem)) ->
                   Vect n (Vect (S k) elem)
transpose_helper [] [] = []
transpose_helper (x :: xs) (y :: ys) = (x :: y) :: transpose_helper xs ys

transpose_mat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transpose_mat [] = create_empties
transpose_mat (x :: xs) = let xs_trans = transpose_mat xs in
                              zipWith (::) x xs_trans

-- *ex_3_3_3> transpose_mat [[1,2], [3,4], [5,6]]
-- [[1, 3, 5], [2, 4, 6]] : Vect 2 (Vect 3 Integer)

-- zipWith : (a -> b -> c) -> Vect n a -> Vect n b -> Vect n c
