module Rosalind.Stronghold.GC where
import Data.List (maximumBy)
import Data.List.Split (splitOn)
import Rosalind.FASTA (parse)


gcContent (label, dna) = gcContent' 0 0 dna
  where
    gcContent' x total [] = (label, 100 * x / total)
    gcContent' x total (b:bs) = gcContent' (x + (isGC b)) (total + 1) bs

    isGC b =
      if b == 'G' || b == 'C'
      then 1
      else 0

maxGC = maximumBy cmpGC
  where
    cmpGC (l1, x1) (l2, x2) = compare x1 x2

solve args = do
  source <- readFile (head args)
  let
    processedEntries = map gcContent $ parse source
    highest = maxGC processedEntries
  return $ pretty highest
  where
    pretty (label, x) = label ++ "\n" ++ (show x)
