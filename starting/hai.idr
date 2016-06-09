module Main

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

-- idris hai.idr
-- hai> valToString False "yaay"
-- "yaay" : String
-- *hai> valToString True 2
-- "2" : String
