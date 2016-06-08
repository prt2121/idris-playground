module Prims

data MyList a = Nil | (:::) a (MyList a)

infixr 10 :::

-- *Prims> 1 ::: Nil
-- 1 ::: [] : MyList Integer
-- *Prims> 0 ::: 1 ::: Nil
-- 0 ::: 1 ::: [] : MyList Integer

-- Idris requires type declarations, using a single colon :
head' : MyList a -> Maybe a
head' Nil = Nothing
head' (x ::: xs) = Just x

-- *Prims> :let ls : MyList Nat; ls = 0 ::: 1 ::: Nil
-- *Prims> head' ls
-- Just 0 : Maybe Nat

-- data Nat    = Z   | S Nat        Natural numbers (zero and successor)
-- Z : Nat                          data constructors

plus' : Nat -> Nat -> Nat
plus' Z     y = y
plus' (S k) y = S (plus' k y)

x : Int
x = 123

foo : String
foo = "boo"

bar : Char
bar = 'X'

bool : Bool
bool = True
