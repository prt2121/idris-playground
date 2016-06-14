module wordlen

import Prelude
import Data.Vect

word_lengths' : Vect k String -> Vect k Nat
word_lengths' [] = [] -- Ctrl-Alt-S = search for a valid expression which satisfies that type
word_lengths' (x :: xs) = Strings.length x :: word_lengths' xs

word_lengths : List String -> List Nat
word_lengths [] = []
word_lengths (x :: xs) = Strings.length x :: word_lengths xs

isEven : Nat -> Bool
isEven Z = True
isEven (S k) = not $ isEven k
