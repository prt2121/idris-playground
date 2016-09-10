module Main

import Effects
import Effect.Exception
import Effect.State
import Effect.StdIO

import LispVal
import Eval


repl : EnvCtx -> IO ()
repl env = do
  putStr ">>> "
  exp <- getLine
  e <- evalExpr env exp
  repl e


main : IO ()
main = repl primEnv
