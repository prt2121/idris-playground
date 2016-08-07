module chap07

-- 7.1.6 Exercises
data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

Eq Shape where
  (==) (Triangle x y) (Triangle x' y') = x == x' && y == y'
  (==) (Rectangle x y) (Rectangle x' y') = x == x' && y == y'
  (==) (Circle d) (Circle d') = d == d'
  (==) _ _ = False

area : Shape -> Double
area (Triangle x y) = x * y / 2
area (Rectangle x y) = x * y
area (Circle x) = pi * x * x

Ord Shape where
    compare x y = compare (area x) (area y)

data Expr num = Val num
              | Add (Expr num) (Expr num)
              | Sub (Expr num) (Expr num)
              | Mul (Expr num) (Expr num)
              | Div (Expr num) (Expr num)
              | Abs (Expr num)

eval : (Neg num, Integral num) => Expr num -> num
eval (Val x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Abs x) = abs (eval x)

-- interface Show a where
--  show : a -> String

-- 7.2.4 Exercises

Show ty => Show (Expr ty) where
    show (Val x) = " " ++ show x ++ " "
    show (Add x y) = "(Add " ++ show x ++ show y ++ ")"
    show (Sub x y) = "(Sub " ++ show x ++ show y ++ ")"
    show (Mul x y) = "(Mul " ++ show x ++ show y ++ ")"
    show (Div x y) = "(Div " ++ show x ++ show y ++ ")"
    show (Abs x) = "(Abs " ++ show x ++ ")"
    -- showPrec d x = ?Show_rhs_2

(Neg ty, Integral ty, Eq ty) => Eq (Expr ty) where
    (==) x y = (==) (eval x) (eval y)

Num ty => Num (Expr ty) where
  (+) = Add
  (*) = Mul
  fromInteger = Val . fromInteger

Neg ty => Neg (Expr ty) where
  negate x = 0 - x
  (-) = Sub
  abs = Abs

  -- *chap07> Circle 2 == Circle 2
  -- True : Bool
  -- *chap07> Circle 2 == Circle 3
  -- False : Bool
  -- *chap07> Circle 2 == Rectangle 1 3
  -- False : Bool

-- counts the number of occurrences of a specific value, of some generic type ty, in a list
occurrences : Eq ty => (item : ty) -> (values : List ty) -> Nat
occurrences item [] = 0
occurrences item (x :: xs) = if item == x
                             then 1 + occurrences item xs
                             else occurrences item xs
