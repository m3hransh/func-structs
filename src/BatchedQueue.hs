module BatchedQueue (BatchedQueue) where
  import Prelude hiding (head, tail)
  import Queue 

  data BatchedQueue a = BQ [a] [a]

  check :: [a] -> [a] -> BatchedQueue a
  check [] r = BQ (reverse r) []
  check f r = BQ f r 

  instance Queue BatchedQueue where
    empty = BQ [] []
    isEmpty (BQ f _) = null f

    snoc (BQ f r) x = check f (x : r)

    head (BQ [] _) = error "empty queue"
    head (BQ (x : _) _) = x

    tail (BQ [] _) = error "empty queue"
    tail (BQ (_ : f) r) = check f r
