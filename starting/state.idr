module States

import Effects
import Effect.State
import Control.Monad.Identity

data Tree a = Leaf
            | Node (Tree a) a (Tree a)

t1 : Tree String
t1 = Node (Node Leaf "One" (Node Leaf "Two" Leaf))
          "Three"
          (Node (Node Leaf "Four" Leaf) "Five" Leaf)

label : Tree a -> Eff (Tree (Int, a)) [STATE Int]
label Leaf = pure Leaf
label (Node l x r) = do l' <- label l
                        lbl <- get
                        put (lbl + 1)
                        r' <- label r
                        return (Node l' (lbl, x) r')

labelFrom : (i : Int) -> Tree a -> Tree (Int, a)
labelFrom i x = runPure (do put i; label x)

-- *state> labelFrom 1 t1
-- Node (Node Leaf (1, "One") (Node Leaf (2, "Two") Leaf))
--      (3, "Three")
--      (Node (Node Leaf (4, "Four") Leaf) (5, "Five") Leaf) : Tree (Int, String)
