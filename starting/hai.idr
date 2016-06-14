module Main

id' : t -> t
id' x = x

-- type Dependent version of id
-- taking an explicit type as its first argument
the' : (t : Type) -> t -> t
the' t x = x

-- *hai> the' (Int)
-- the' Int : Int -> Int
-- *hai> the' (Int) 1
-- 1 : Int
-- *hai> id' 1
-- 1 : Integer

main : IO ()
main = putStrLn "Oh, hai"

StringOrIntType : Bool -> Type
StringOrIntType x = case x of
                  True => Int
                  False => String

stringOrIntVal : (x : Bool) -> StringOrIntType x
stringOrIntVal x = case x of
                      True => 7
                      False => "Seven"

valToString : (x : Bool) -> StringOrIntType x -> String
valToString x val = case x of
                        True => cast val
                        False => val

valToStringWithHole : (x : Bool) -> StringOrIntType x -> String
valToStringWithHole x val = case x of
                        True => ?xTrueType -- holes
                        False => ?xFalseType -- ctrl + alt + t

-- idris hai.idr
-- hai> valToString False "yaay"
-- "yaay" : String
-- *hai> valToString True 2
-- "2" : String
