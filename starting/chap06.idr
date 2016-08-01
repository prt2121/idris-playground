module chap06

import Data.Vect

AdderType : (numargs : Nat) -> Type
AdderType Z = Int
AdderType (S k) = (next : Int) -> AdderType k

adder : (numargs : Nat) -> (acc : Int) -> AdderType numargs
adder Z acc = acc
adder (S k) acc = \next => adder k (next + acc)

GenericAdderType : (numargs : Nat) -> Type -> Type
GenericAdderType Z numType = numType
GenericAdderType (S k) numType = (next : numType) -> GenericAdderType k numType

genericAdder : Num numType => (numargs : Nat) -> numType -> GenericAdderType numargs numType
genericAdder Z acc = acc
genericAdder (S k) acc = \next => genericAdder k (next + acc)

Position : Type
Position = (Double, Double)

Polygon : Nat -> Type
Polygon n = Vect n Position

tri : Polygon 3
tri = [(0, 0), (1, 1), (-1, -1)]
-- tri = [(?tri_rhs1, ?tri_rhs2), (?tri_rhs3, ?tri_rhs4), (?tri_rhs5, ?tri_rhs6)]

IntOrString : Bool -> Type
IntOrString False = String
IntOrString True = Int

-- dependent pattern matching
getIntOrString : (isInt : Bool) -> IntOrString isInt
getIntOrString False = "zero"
getIntOrString True = 0

-- converts either a String or an Int to a canonical String representation
valToString : (isInt : Bool) -> IntOrString isInt -> String
valToString False x = trim x
valToString True x = cast x

valToString' : (isInt : Bool) -> (case isInt of
                                       False => String
                                       True => Int) -> String
valToString' False x = trim x
valToString' True x = cast x
