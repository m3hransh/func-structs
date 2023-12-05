module Main where

import BatchedQueue
import Heap
import LeftistHeap
import Queue
import Test.HUnit
import Prelude hiding (head, tail)

-- test1 :: Test
-- test1 = TestCase (assertEqual "Deque first element of singlton queue" 3 (head (snoc (empty :: BatchedQueue Integer) 3)))

-- tests :: Test
-- tests = TestList [TestLabel "test1" test1]

-- Helper function to convert a LeftistHeap to a list
heapToList :: LeftistHeap Int -> [Int]
heapToList E = []
heapToList (T _ x left right) = x : heapToList left ++ heapToList right

queueToList :: BatchedQueue Int -> [Int]
queueToList (BQ f r) = f ++ reverse r

tests :: Test
tests =
  TestList
    [ ------ BatchedQueue Test Cases
      -- Test case for inserting elements to queue
      "BatchedQueue insertion"
        ~: queueToList (Queue.snoc (Queue.snoc (Queue.snoc Queue.empty 3) 1) 2)
        ~?= [3, 1, 2],
      -- Test case for geting the first element
      "BatchedQueue head"
        ~: Queue.head (Queue.snoc (Queue.snoc (Queue.snoc (Queue.empty :: (BatchedQueue Int)) 3) 1) 2)
        ~?= 3,
      "BatchedQueue tail"
        ~: (queueToList . tail) (Queue.snoc (Queue.snoc (Queue.snoc (Queue.empty :: (BatchedQueue Int)) 3) 1) 2)
        ~?= [1, 2],
      ------ LeftistHeap Test Cases
      -- Test case for inserting elements
      "LeftisHeap Insertion"
        ~: heapToList (Heap.insert 3 (Heap.insert 1 (Heap.insert 2 Heap.empty)))
        ~?= [1, 2, 3],
      -- Test case for merging two heaps
      "LeftistHeap Merge"
        ~: heapToList (merge (Heap.insert 3 Heap.empty) (Heap.insert 1 (Heap.insert 2 Heap.empty)))
        ~?= [1, 2, 3],
      -- Test case for finding the minimum element
      "LeftistHeap FindMin"
        ~: findMin (Heap.insert 3 (Heap.insert 1 (Heap.insert 2 Heap.empty :: LeftistHeap Int)))
        ~?= 1,
      -- Test case for deleting the minimum element
      "Leftist Heap DeleteMin"
        ~: heapToList (deleteMin (Heap.insert 3 (Heap.insert 1 (Heap.insert 2 Heap.empty))))
        ~?= [2, 3]
    ]

main :: IO Counts
main = runTestTT tests
