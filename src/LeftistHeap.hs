module LeftistHeap (LeftistHeap (..)) where

import Heap

data LeftistHeap a = E | T Int a (LeftistHeap a) (LeftistHeap a)

rank :: LeftistHeap a -> Int
rank E = 0
rank (T r _ _ _) = r

makeT :: a -> LeftistHeap a -> LeftistHeap a -> LeftistHeap a
makeT x a b =
  if rank a >= rank b
    then T (rank b + 1) x a b
    else T (rank a + 1) x b a

instance Heap LeftistHeap where
  empty = E
  isEmpty E = True
  isEmpty _ = False

  insert x = merge (T 1 x E E)

  merge h E = h
  merge E h = h
  merge h1@(T _ x a1 b1) h2@(T _ y a2 b2) =
    if x <= y
      then makeT x a1 (merge b1 h2)
      else makeT y a2 (merge h1 b2)

  findMin E = error "empty heap"
  findMin (T _ x _ _) = x

  deleteMin E = error "empty heap"
  deleteMin (T _ _ a b) = merge a b
