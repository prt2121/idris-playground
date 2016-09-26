module Labelling

import Control.Monad.State

-- 12.1.4 Exercises
update : (stateType -> stateType) -> State stateType ()
update f = do state <- get
              put $ f state

increase' : Nat -> State Nat ()
increase' inc = update (+inc)

-- *Labelling> runState (increase' 5) 89
-- ((), 94) : ((), Nat)

-- Tree Labelling

data Tree a = Empty
              | Node (Tree a) a (Tree a)


countEmpty : Tree a -> State Nat ()
countEmpty Empty = do update (+ 1)
countEmpty (Node l v r) = do countEmpty l
                             countEmpty r


countEmptyNode : Tree a -> State (Nat, Nat) ()
countEmptyNode Empty = do update (\p => (fst p, snd p + 1))
countEmptyNode (Node l v r) = do update (\p => (fst p + 1, snd p))
                                 countEmptyNode l
                                 countEmptyNode r

 -- *Labelling> execState (countEmpty testTree) 0
 -- 7 : Nat

testTree : Tree String
testTree = Node (Node (Node Empty "Jim" Empty) "Fred"
                      (Node Empty "Sheila" Empty)) "Alice"
                (Node Empty "Bob" (Node Empty "Eve" Empty))


flatten : Tree a -> List a
flatten Empty = []
flatten (Node left val right) = flatten left ++ val :: flatten right

treeLabelWith : Stream labelType ->
                Tree a ->
                (Stream labelType, Tree (labelType, a))
treeLabelWith lbls Empty = (lbls, Empty)
treeLabelWith lbls (Node left val right)
    = let (this :: lblsLeft, left_labelled) = treeLabelWith lbls left
          (lblsRight, right_labelled)       = treeLabelWith lblsLeft right
      in
          (lblsRight, Node left_labelled (this, val) right_labelled)

treeLabel : Tree a -> Tree (Integer, a)
treeLabel tree = snd (treeLabelWith [1..] tree)

increase : Nat -> State Nat ()
increase inc = do current <- get
                  put (current + inc)

tLabelWith : Tree a -> State (Stream labelType) (Tree (labelType, a))
tLabelWith Empty = pure Empty
tLabelWith (Node left val right)
                 = do left_labelled <- tLabelWith left
                      (h :: t) <- get
                      put t
                      right_labelled <- tLabelWith right
                      pure (Node left_labelled (h, val) right_labelled)
