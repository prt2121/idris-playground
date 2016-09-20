module equality

import Data.Vect

-- 8.1.7 Exercises
same_cons : {xs : List a}
            -> {ys : List a}
            -> xs = ys
            -> x :: xs = x :: ys
same_cons Refl = Refl


same_lists : {xs : List a}
             -> {ys : List a}
             -> x = y
             -> xs = ys
             -> x :: xs = y :: ys
same_lists Refl Refl = Refl


data EqNat : (num1 : Nat) -> (num2 : Nat) -> Type where
     Same : (num : Nat) -> EqNat num num

-- Functions as Proofs: Manipulating Equalities
sameS : (k : Nat) -> (j : Nat) -> (eq : EqNat k j) -> EqNat (S k) (S j)
sameS j j (Same j) = Same (S j)


checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqNat Z Z = Just (Same Z)
checkEqNat Z (S k) = Nothing
checkEqNat (S k) Z = Nothing
checkEqNat (S k) (S j) = case checkEqNat k j of
                              Just eq => Just $ sameS _ _ eq
                              Nothing => Nothing


checkEqNat' : (num1 : Nat) -> (num2 : Nat) -> Maybe (num1 = num2)
checkEqNat' Z Z = Just Refl
checkEqNat' Z (S k) = Nothing
checkEqNat' (S k) Z = Nothing
checkEqNat' (S k) (S j) = case checkEqNat' k j of
                               Just eq => Just $ cong eq
                               Nothing => Nothing


exactLength : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLength {m} len input = case checkEqNat m len of
                                 Just (Same len) => Just input
                                 Nothing => Nothing
