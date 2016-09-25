module Hangman

import Data.Vect
import Pred


data GameState : (guesses_remaining : Nat) -> (letters : Nat) -> Type where
     MkGameState : (word : String)
                   -> (missing : Vect letters Char)
                   -> GameState guesses_remaining letters


data Finished : Type where
     Lost : (game : GameState 0 (S letters)) -> Finished
     Won : (game : GameState (S guesses) 0) -> Finished


data ValidInput : List Char -> Type where
     Letter : (c : Char) -> ValidInput [c]


processGuess : (letter : Char) ->
               GameState (S guesses) (S letters) ->
               Either (GameState guesses (S letters)) (GameState (S guesses) letters)
processGuess letter (MkGameState word missing)
                = case isElem letter missing of
                       Yes prf => Right (MkGameState word (removeElem_auto letter missing))
                       No contra => Left (MkGameState word missing)


isValidInput : (cs : List Char) -> Dec (ValidInput cs)


isValidString : (s : String) -> Dec (ValidInput (unpack s))


readGuess : IO (x ** ValidInput x)
readGuess = do putStr "Guess:"
               x <- getLine
               case isValidString (toUpper x) of
                    Yes prf => pure (_ ** prf)
                    No contra => do putStrLn "Invalid guess"
                                    readGuess

game : GameState (S guesses) (S letters) -> IO Finished
game st = ?game_rhs
