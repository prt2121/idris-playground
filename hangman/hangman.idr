module hangman

import Effects

-- State

data GState = Running Nat Nat | NotRunning
data Hangman : GState -> Type

-- Rules

data GRules : Effect where
  Guess : (c : Char) ->
          sig GRules Bool
              (Hangman (Running (S g) (S w)))
              (\inword => if inword
                          then Hangman (Running (S g) w)
                          else Hangman (Running g (S w)))
  Won  : sig GRules () (Hangman (Running g 0))
                       (Hangman NotRunning)
  Lost : sig GRules () (Hangman (Running 0 g))
                       (Hangman NotRunning)
  Get : sig GRules String (Hangman h)
