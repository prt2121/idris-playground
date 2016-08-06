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
