module Rosalind.Stronghold.HAMM where


distance :: String -> String -> Integer
distance xs ys = distance' 0 xs ys
  where
    distance' k [] []         = k
    distance' k [] (y:ys)     = distance' (k + 1) [] ys
    distance' k (x:xs) []     = distance' (k + 1) xs []
    distance' k (x:xs) (y:ys) =
      if x == y
      then distance' k xs ys
      else distance' (k + 1) xs ys

parse :: String -> [String]
parse raw = take 2 $ lines raw

solve args = do
  source <- readFile (head args)
  return $ show $ hamm $ parse source
  where
    hamm (dna1:dna2:[]) = distance dna1 dna2
