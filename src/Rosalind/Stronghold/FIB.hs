module Rosalind.Stronghold.FIB where

fibonacci n k = head $ reverse $ take n (fibonacciSeries k)

fibonacciSeries k = [1, 1] ++ (generateFibonacci 1 1)
  where
    generateFibonacci a b = (b + a * k):(generateFibonacci b (b + a * k))

solve (a1:a2:args) = return $ show $ fibonacci n k
  where
    n = read a1 :: Int
    k = read a2 :: Int
