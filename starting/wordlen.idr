module wordlen

import Prelude

-- Interactive Editing in Atom
word_lengths : List String -> List Nat
word_lengths [] = [] -- ?word_lengths_rhs_1
word_lengths (x :: xs) = Strings.length x :: word_lengths xs -- ?word_lengths_rhs_2

-- *wordlen> word_lengths ["1", "12"]
-- [1, 2] : List Nat

isEven : Nat -> Bool
isEven Z = True
isEven (S k) = not $ isEven k
