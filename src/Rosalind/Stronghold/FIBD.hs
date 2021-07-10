module Rosalind.Stronghold.FIBD where

fibonacci n m = last $ take n (fibonacciSeries m)

fibonacciSeries m = [1, 1] ++ (generateFibonacci 1 1 m [1, 1])

-- generateFibonacci is an infinite generator of fibonacci numbers.
-- a & b are the previous two generations.
-- m is the number of iterations before a generation dies.
-- history is a list of the past generation states.
generateFibonacci a b m history = pop:(generateFibonacci b pop m history')
  where
    -- dying is the number that are dying
    dying =
      if (length history) < m
      then 0
      else last history
    -- pop is the next generation
    pop = b + a - dying
    -- history' is a list of the states of the last m+1 populations
    history' = take (m + 1) (pop:history)

solve (a1:a2:args) = return $ show $ fibonacci n m
  where
    n = read a1 :: Int
    m = read a2 :: Int
