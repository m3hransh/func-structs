module Main where

import BatchedQueue
import Queue
import Prelude hiding (head, tail)

main :: IO ()
main = do
  let q = empty :: BatchedQueue Int
  let q1 = snoc q 1
  let q2 = snoc q1 2
  print $ head q2
  let q3 = tail q2
  print $ head q3
