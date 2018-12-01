module Rosalind.Stronghold.RNA where

toRna dna = map subU dna
  where
    subU 'T' = 'U'
    subU c = c

solve args = toRna (head args)
