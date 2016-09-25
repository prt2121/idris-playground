module ex

-- ex 9.1.6

-- List version of Elem dependent type
data Elem : a -> List a -> Type where
     Here : Elem x (x::xs)
     There : (later : Elem x xs) -> Elem x (y::xs)


data Last : List a -> a -> Type where
     LastOne : Last [value] value
     LastCons : (prf : Last xs value) -> Last (x :: xs) value


notInNil : Last [] value -> Void
notInNil LastOne impossible
notInNil (LastCons _) impossible


single : (contra : (x = value) -> Void) -> Last [x] value -> Void
single contra LastOne = contra Refl
single _ (LastCons LastOne) impossible
single _ (LastCons (LastCons _)) impossible


noGood : (contra : Last (y :: ys) value -> Void) -> Last (x :: (y :: ys)) value -> Void
noGood contra (LastCons prf) = contra prf


isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast [] value = No notInNil
isLast (x :: []) value = case decEq x value of
                              (Yes Refl) => Yes LastOne
                              (No contra) => No (single contra)
isLast (x :: (y :: ys)) value = case isLast (y :: ys) value of
                                     (Yes prf) => Yes (LastCons prf)
                                     (No contra) => No (noGood contra)
