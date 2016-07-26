-- 4.1.5 Exercises

-- integer arithmetic expression
data Exp = Single Int
         | Add Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp

-- more flexible syntax
-- data Exp : Type where
--      Single : Int -> Exp
--      Add : Exp -> Exp -> Exp
--      Sub : Exp -> Exp -> Exp
--      Mul : Exp -> Exp -> Exp

eval : Exp -> Int
eval (Single x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y

-- *Chap4> eval (Add (Single 1) (Single 2))
-- 3 : Int

data BSTree : Type -> Type where
      Empty : Ord e => BSTree e
      Node  : Ord e => (left : BSTree e) -> (val : e) -> (right : BSTree e) -> BSTree e

-- %name BSTree left, right
-- using a %name directive to give naming hints for building definitions interactively

insert : elem -> BSTree elem -> BSTree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right) = case compare x val of
                                    LT => Node (insert x left) val right
                                    EQ => orig -- same as as-patterns in Haskell
                                    GT => Node left val (insert x right)
-- match Ctrl-Alt-M

listToTree : Ord a => List a -> BSTree a
listToTree [] = Empty
listToTree (x :: xs) = insert x $ listToTree xs

-- *starting/Chap4> listToTree ['p', 'r', 'a', 't']
-- Node (Node Empty 'a' (Node (Node Empty 'p' Empty) 'r' Empty)) 't' Empty : BSTree Char

treeToList : BSTree a -> List a
treeToList Empty = []
treeToList (Node left val right) = treeToList left ++ [val] ++ treeToList right
