module Console

import Effects
import Effect.StdIO
import Effect.State

Names : Type
Names = List String

name : Eff () [STATE Names, STDIO]
name = do putStr "Name? "
          n <- getStr
          case n of
            ""        => name
            "bye!"    => putStrLn "bye"
            otherwise => do names <- get
                            put $ n :: names
                            putStrLn $ show !get
                            putStrLn $ "hi, " ++ n
                            name

main : IO ()
main = run name

-- *src/Console> :exec main
-- Name? John
-- ["John"]
-- hi, John
-- Name? Jim
-- ["Jim", "John"]
-- hi, Jim
-- Name? Jane
-- ["Jane", "Jim", "John"]
-- hi, Jane
-- Name? Jerry
-- ["Jerry", "Jane", "Jim", "John"]
