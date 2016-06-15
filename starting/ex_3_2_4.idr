module ex_3_2_4

import Data.Vect

-- interactive editing mode
-- ctrl+alt+a
-- ctrl+alt+c
-- ctrl+alt+l
-- ctrl+alt+s

map' : (a -> b) -> List a -> List b
map' f [] = []
map' f (x :: xs) = f x :: map' f xs

map'' : (a -> b) -> Vect n a -> Vect n b
map'' f [] = []
map'' f (x :: xs) = f x :: map'' f xs

length' : List a -> Nat
length' [] = 0
length' (x :: xs) = S $ length' xs

-- *ex_3_2_4> length' []
-- (input):Can't infer argument a to length', Can't infer argument a to []
-- Can't infer argument
-- need explicit type, using `the`
-- *ex_3_2_4> length' $ the (List Int) []
-- 0 : Nat

reverse' : List a -> List a
reverse' [] = []
reverse' (x :: xs) = reverse' xs ++ [x]

-- *ex_3_2_4> reverse' [1,2,3]
-- [3, 2, 1] : List Integer
