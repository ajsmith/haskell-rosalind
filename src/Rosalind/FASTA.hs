module Rosalind.FASTA
  (parse)
where
import Data.List.Split (splitOn)


{- Parse FASTA entries from a string, returning a list of String pairs
of (ID, data).

For more information on FASTA format, see:
<https://en.wikipedia.org/wiki/FASTA_format>

-}
parse :: String -> [(String, String)]
parse raw = [parseFields (lines e) | e<-(tail $ splitOn ">" raw)]
  where
    parseFields :: [String] -> (String, String)
    parseFields (l:ls) = (l, (concat ls))


solve :: [String] -> IO String
solve args = do
  source <- readFile (head args)
  return $ show $ parse source
