module Rosalind.Stronghold.RNA where

toRna dna = map subU dna
  where
    subU 'T' = 'U'
    subU c = c

solve args = return $ toRna (head args)
