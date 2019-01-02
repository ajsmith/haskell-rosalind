module Rosalind.Stronghold.CONS where
import Data.List (sort)
import qualified Rosalind.FASTA (parse)

-- Determine the profile matrix and consensus string for homologous
-- DNA strands.
--
-- <http://rosalind.info/problems/cons/>


-- Return a profile matrix of a matrix of DNA strands.
profile = transpose . transposedProfile

-- Return a transposed profile matrix of a matrix of DNA strands.
transposedProfile matrix = map profileRow (transpose matrix)
  where
    -- Return the profile of a row in a matrix.
    profileRow = foldr countBase [0, 0, 0, 0]

-- Transpose a matrix.
-- Example:
-- [A B] => [A C E]
-- [C D]    [B D F]
-- [E F]
transpose matrix = reverse $ transpose' matrix []
  where
    transpose' xs ys =
      if elem [] xs
      then ys
      else transpose' (map tail xs) ((map head xs):ys)

-- Increment the count for a DNA nucleotide base.
countBase :: Char -> [Int] -> [Int]
countBase 'A' (a:c:g:t:[]) = [a + 1, c, g, t]
countBase 'C' (a:c:g:t:[]) = [a, c + 1, g, t]
countBase 'G' (a:c:g:t:[]) = [a, c, g + 1, t]
countBase 'T' (a:c:g:t:[]) = [a, c, g, t + 1]

-- Return the DNA base with the highest count.
-- Expects a list with the following counts in this order: [A, C, G, T]
maxBase pRow = snd $ head $ reverse $ sort $ zip pRow "ACGT"

-- Return the consensus of a profile matrix.
consensus pMatrix = map maxBase pMatrix

-- Return a list of strings which render a profile matrix.
displayProfile matrix = prefix ls
  where
    ls = map (unwords . map show) matrix
    prefix (l1:l2:l3:l4:[]) = [
      "A: " ++ l1,
      "C: " ++ l2,
      "G: " ++ l3,
      "T: " ++ l4
      ]

solve :: [String] -> IO String
solve args = do
  source <- readFile (head args)
  let
    tpMatrix = transposedProfile $ parse source
    result = (consensus tpMatrix):(displayProfile $ transpose tpMatrix)
  return $ unlines result
  -- return $ show $ parse source
  where
    -- Parse FASTA source into a DNA matrix.
    parse = toMatrix . Rosalind.FASTA.parse

    -- Arrange DNA from FASTA entries as a matrix.
    toMatrix fastaEntries = [snd e | e<-fastaEntries]
