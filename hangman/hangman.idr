module hangman

import Effects

-- state

data GState = Running Nat Nat | NotRunning
data Hangman : GState -> Type

-- rules

data GRules : Effect where
     Guess : (x : Char) ->
             sig GRules Bool
                 (Hangman (Running (S g) (S w)))
                 (\inword => if inword
                             then Hangman (Running (S g) w)
                             else Hangman (Running g (S w)))
     Won  : sig GRules () (Hangman (Running g 0))
                                $ Hangman NotRunning
     Lost : sig GRules () (Hangman (Running 0 g))
                                $ Hangman NotRunning
     NewWord : (w : String) ->
               sig GRules () (Hangman NotRunning) $ Hangman $ Running 6 $ length $ unpack w
     Get : sig GRules String $ Hangman h
