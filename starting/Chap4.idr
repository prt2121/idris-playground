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
