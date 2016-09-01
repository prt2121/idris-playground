module Main

import Effects
import Effect.StdIO
import Effect.Exception
import Effect.State

import Light
import Types

until_ : (pred : String -> Bool)
         -> (prompt : Eff String [STDIO])
         -> (action : String -> Eff () [STATE Env, STDIO, EXCEPTION Error])
         -> (Eff () [STATE Env, STDIO, EXCEPTION Error])
until_ pred prompt action = do
                              result <- prompt
                              if pred result
                                then pure ()
                                else do action result
                                        until_ pred prompt action

-- prints out a prompt and reads in a line of input
readPrompt : String -> Eff String [STDIO]
readPrompt prompt = do putStr prompt
                       getStr

-- todo: catch errors
evalString : String -> Eff String [STATE Env, EXCEPTION Error]
evalString expr = pure $ show !(evaluate expr)

-- lose states
-- evalAndPrint : String -> Eff () [STATE Env, STDIO, EXCEPTION Error]
-- evalAndPrint expr =  do case run (evalString expr) of
--                              Left e => putStrLn $ show e
--                              Right s => putStrLn s

evalAndPrint : String -> Eff () [STATE Env, STDIO, EXCEPTION Error]
evalAndPrint expr =  do s <- evalString expr
                        putStrLn s

runRepl : Eff () [STATE Env, STDIO, EXCEPTION Error]
runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint

go : List String -> IO ()
go [_] = run runRepl
go [_, expr] = run $ evalAndPrint expr
go _ = do putStrLn "Program takes only 0 or 1 argument"

main : IO ()
main = do args <- getArgs
          go args
