module Rosalind.Stronghold.DNA where

baseFrequency a c g t [] = [a, c, g, t]
baseFrequency a c g t (x:xs)
  | x == 'A' = baseFrequency (a + 1) c g t xs
  | x == 'C' = baseFrequency a (c + 1) g t xs
  | x == 'G' = baseFrequency a c (g + 1) t xs
  | x == 'T' = baseFrequency a c g (t + 1) xs

solve args = return $ prettyPrint $ baseFrequency 0 0 0 0 (head args)
  where
    prettyPrint xs = unwords $ map show xs
