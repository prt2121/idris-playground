data Tree elem = Empty
                | Node (Tree elem) elem (Tree elem)
%name Tree tree, tree1
-- using a %name directive to give naming hints for building definitions interactively

insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right) = case compare x val of
                                    LT => Node (insert x left) val right
                                    EQ => orig -- same as as-patterns in Haskell
                                    GT => Node left val (insert x right)
-- match Ctrl-Alt-M
