module Main

import Effects
import Effect.StdIO
import Effect.Exception

import Light
import Types

until_ : (String -> Bool) -> (Eff String [STDIO]) -> (String -> Eff () [STDIO, EXCEPTION Error]) -> (Eff () [STDIO, EXCEPTION Error])
until_ pred prompt action = do
                              result <- prompt
                              if pred result
                                then return ()
                                else do action result
                                        until_ pred prompt action

-- prints out a prompt and reads in a line of input
readPrompt : String -> Eff String [STDIO]
readPrompt prompt = do putStr prompt
                       getStr

evalString : String -> Eff String [EXCEPTION Error]
evalString expr = return $ show !(evaluate expr)

evalAndPrint : String -> Eff () [STDIO, EXCEPTION Error]
evalAndPrint expr =  do s <- evalString expr
                        putStrLn s

runRepl : Eff () [STDIO, EXCEPTION Error]
runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint

go : List String -> IO ()
go [_] = run runRepl
go [_, expr] = run $ evalAndPrint expr
go _ = do putStrLn "Program takes only 0 or 1 argument"

main : IO ()
main = do args <- getArgs
          go args
