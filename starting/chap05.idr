module Chap05

import System
import Data.Vect

printLength : IO ()
printLength = getLine >>= \line => let len = length line in
                                   putStrLn $ show len

-- 5.1.4 Exercises
-- Using do notation, write a program which reads two strings then displays the length of the longer string
lenghtOfLonger : IO ()
lenghtOfLonger = do
                    line1 <- getLine
                    line2 <- getLine
                    let l1 = length line1
                    let l2 = length line2
                    putStrLn $ show $ if l1 > l2 then l1 else l2

-- Write the same program using >>= instead of do notation
lenghtOfLonger' : IO ()
lenghtOfLonger' = getLine >>=
                    \line1 => let l1 = length line1 in
                              getLine >>=
                                \line2 => let l2 = length line2 in
                                          putStrLn $ show $ if l1 > l2 then l1 else l2

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit $ unpack input
    then pure $ Just $ cast input
    else pure Nothing

-- 5.2.4 Exercises
-- Write a function which implements a simple "guess the number" game.
guess : (target : Nat) -> IO ()
guess target = do
                  putStrLn "your guess : "
                  Just ok <- readNumber | Nothing => do
                                                      putStrLn "number please"
                                                      guess target
                  case compare ok target of
                    LT => do
                            putStrLn "too low"
                            guess target
                    GT => do
                            putStrLn "too high"
                            guess target
                    EQ => putStrLn "bingo!"

read_vect : IO (len ** Vect len String)
read_vect = do l <- getLine
               if l == ""
                  then pure (_ ** [])
                  else do (_ ** ls) <- read_vect
                          pure (_ ** l :: ls)

zipInputs : IO ()
zipInputs = do putStrLn "Enter first vector (blank line to end):"
               (len1 ** vec1) <- read_vect
               putStrLn "Enter second vector (blank line to end):"
               (len2 ** vec2) <- read_vect
               case exactLength len1 vec2 of
                    Nothing => putStrLn "Vectors are different lengths"
                    Just vec2' => printLn (zip vec1 vec2')

-- 5.3.5 Exercises
-- openFile, closeFile, fEOF, fGetLine, writeFile
readToBlank : IO (List String)
readToBlank = do l <- getLine
                 if l == ""
                    then pure []
                    else do ls <- readToBlank
                            pure $ l :: ls

-- *chap05> :exec readToBlank >>= printLn
-- first
-- second
--
-- ["first", "second"]

-- openFile	 : 	(f : String) -> (m : Mode) -> IO File
-- openFile	 : 	(f : String) -> (m : Mode) -> IO (Either FileError File)
-- writeFile	 : 	(filepath : String) -> (contents : String) -> IO (Either FileError ())

readAndSave : IO ()
readAndSave =
  do ls <- readToBlank
     filename <- getLine
     Right result <- writeFile filename (unwords ls)
      | Left err => putStrLn (show err)
     pure result

main : IO ()
main = readAndSave

-- *chap05> :exec zipInputs
-- Enter first vector (blank line to end):
-- 1
-- 2
-- 3
--
-- Enter second vector (blank line to end):
-- a
-- b
-- c
--
-- [("1", "a"), ("2", "b"), ("3", "c")]

-- *chap05> :exec read_vect >>= printLn
-- hi
-- howdy
--
-- (2 ** ["hi", "howdy"])

-- main : IO ()
-- main = do
--         r <- random
--         putStrLn r

-- *chap05> :exec lenghtOfLonger'
-- long
-- longer
-- 6

-- *chap05> :exec lenghtOfLonger
-- short
-- longer
-- 6

--  ~/W/i/starting git:master λ →  ~/idris-install/.cabal-sandbox/bin/idris chap05.idr                                                                                    ⬆ ✖ ◼
--       ____    __     _
--      /  _/___/ /____(_)____
--      / // __  / ___/ / ___/     Version 0.12
--    _/ // /_/ / /  / (__  )      http://www.idris-lang.org/
--   /___/\__,_/_/  /_/____/       Type :? for help
 --
--  Idris is free software with ABSOLUTELY NO WARRANTY.
--  For details type :warranty.
--  Type checking ./chap05.idr
--  *chap05> :exec printLength
--  haha
--  4
--  *chap05> :q
--  Bye bye
