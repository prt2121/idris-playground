module Console

import Effects
import Effect.StdIO
import Effect.State

name : Eff () [STATE String, STDIO]
name = do putStr "Name? "
          n <- getStr
          before <- get
          putStrLn before
          put n
          putStrLn $ "hi, " ++ n
          name

main : IO ()
main = run name
