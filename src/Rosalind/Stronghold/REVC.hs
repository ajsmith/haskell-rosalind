module Rosalind.Stronghold.REVC where

complementBase 'A' = 'T'
complementBase 'T' = 'A'
complementBase 'C' = 'G'
complementBase 'G' = 'C'

complementStrand = map complementBase

reverseComplement = reverse . complementStrand

solve args = return $ reverseComplement (head args)
