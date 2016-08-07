module exactLength

-- Equality: Expressing Relationships Between Data

data Vect : Nat -> Type -> Type where
  Nil : Vect Z a
  (::) : a -> Vect k a -> Vect (S k) a

-- not good
-- exactLength : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
-- exactLength {m} len input = case m == len of
--                                  False => Nothing
--                                  True => ?exactLength_rhs_2

data EqNat : (num1 : Nat) -> (num2 : Nat) -> Type where
  Same : (num : Nat) -> EqNat num num

-- Functions as Proofs
-- sameS : EqNat k j -> EqNat (S k) (S j)
sameS : (k : Nat) -> (j : Nat) -> (eq : EqNat k j) -> EqNat (S k) (S j)
sameS k k (Same k) = Same (S k)

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqNat Z Z = Just $ Same 0
checkEqNat Z (S k) = Nothing
checkEqNat (S k) Z = Nothing
checkEqNat (S k) (S j) = case checkEqNat k j of
                              Nothing => Nothing
                              Just x => Just (sameS _ _ x)

exactLength : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLength {m} len input = case checkEqNat len m of
                                 Nothing => Nothing
                                 Just (Same m) => Just input

-- data (=) : a -> b -> Type where
--   Refl : x = x

-- cong : {func : a -> b} -> x = y -> func x = func y

-- 8.1.7 Exercises
same_cons : {xs : List a} -> {ys : List a} -> xs = ys -> x :: xs = x :: ys
same_cons Refl = Refl

same_lists : {xs : List a} -> {ys : List a} -> x = y -> xs = ys -> x :: xs = y :: ys
same_lists Refl Refl = Refl

data ThreeEq : a -> b -> c -> Type where
  SameSame : ThreeEq x x x

allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS x x x SameSame = SameSame
