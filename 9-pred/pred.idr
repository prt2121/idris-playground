module Pred

import Data.Vect


maryInVector : Elem "Prat" ["Peter", "Paul", "Prat"]
maryInVector = There $ There Here

export
removeElem : (value : a) ->
             (xs : Vect (S n) a) ->
             (prf : Elem value xs) ->
             Vect n a
removeElem value (value :: ys) Here = ys
removeElem {n = Z} value (y :: []) (There later) = absurd later
removeElem {n = (S k)} value (y :: ys) (There later) = y :: removeElem value ys later

-- absurd : Uninhabited t => t -> a

removeElem_auto : (value : a) ->
             (xs : Vect (S n) a) ->
             {auto prf : Elem value xs} ->
             Vect n a
removeElem_auto value xs {prf} = removeElem value xs prf


-- data Elem : a -> Vect k a -> Type where
--      Here : Elem x (x :: xs)
--      There : (later : Elem x xs) -> Elem x (y :: xs)


not_in_nil : Elem value [] -> Void
not_in_nil Here impossible
not_in_nil (There _) impossible


not_in_tail : (notThere : Elem value xs -> Void) ->
              (notHere : (value = x) -> Void) ->
              Elem value (x :: xs) -> Void
not_in_tail notThere notHere Here = notHere Refl
not_in_tail notThere notHere (There later) = notThere later


isElem : DecEq a => (value : a) -> (xs : Vect n a) -> Dec (Elem value xs)
isElem value [] = No not_in_nil
isElem value (x :: xs) = case decEq value x of
                              Yes Refl => Yes Here
                              No notHere => case Pred.isElem value xs of
                                                 Yes prf => Yes (There prf)
                                                 No notThere => No (not_in_tail notThere notHere)
