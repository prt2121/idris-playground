module chap06_printf

-- intermediate type
data Format = Number Format     -- %d
            | Str Format        -- %s
            | Lit String Format
            | End

PrintfType : Format -> Type
PrintfType (Number f) = (i : Int) -> PrintfType f
PrintfType (Str f) = (str : String) -> PrintfType f
PrintfType (Lit s f) = PrintfType f
PrintfType End = String

-- helper function
printfFmt : (f : Format) -> (acc : String) -> PrintfType f
printfFmt (Number f) acc = \i => printfFmt f (acc ++ show i)
printfFmt (Str f) acc = \str => printfFmt f (acc ++ str)
printfFmt (Lit s f) acc = printfFmt f (acc ++ s)
printfFmt End acc = acc

toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: chars) = Number (toFormat chars)
toFormat ('%' :: 's' :: chars) = Str (toFormat chars)
toFormat ('%' :: chars)        = Lit "%" (toFormat chars)
toFormat (c :: chars)          = case toFormat chars of
                                      Lit lit chars' => Lit (strCons c lit) chars'
                                      fmt => Lit (strCons c "") fmt

printf : (fmt : String) -> PrintfType (toFormat (unpack fmt))
printf fmt = printfFmt _ ""

-- *chap06_printf> :t printf
-- printf : (fmt : String) -> PrintfType (toFormat (unpack fmt))
-- *chap06_printf> printf "hey, %s"
-- \str => prim__concat "hey, " str : String -> String
-- *chap06_printf> printf "hey, %s" "you"
-- "hey, you" : String
