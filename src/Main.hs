module Main where
import System.Environment (getArgs)
import qualified Rosalind.Stronghold.DNA
import qualified Rosalind.Stronghold.RNA


main :: IO ()
main = do
  args <- getArgs
  result <- solve args
  putStrLn result
  where

    solve :: [String] -> IO String
    solve (section:problem:args) = solve' section problem args

    solve' "stronghold" "dna" = Rosalind.Stronghold.DNA.solve
    solve' "stronghold" "rna" = Rosalind.Stronghold.RNA.solve
