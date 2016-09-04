module Main

import Effects
import Effect.Exception
import Effect.State
import Effect.StdIO

import LispVal
import Eval

runTextToEval : IO ()
runTextToEval = do
                  (_ :: expr :: _) <- getArgs
                  case the (Either LispError LispVal) $ run $ textToEval expr of
                       Left e => putStrLn $ show e
                       Right v => putStrLn $ show v


runEvalExp : IO ()
runEvalExp = do
                (_ :: expr :: _) <- getArgs
                evalExpr expr


repl : IO ()
repl = do
  putStr ">>> "
  exp <- getLine
  evalExpr exp
  repl


main : IO ()
main = repl
