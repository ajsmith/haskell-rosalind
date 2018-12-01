module Main where
import System.Environment (getArgs)
import qualified Rosalind.Stronghold.DNA
import qualified Rosalind.Stronghold.RNA


main :: IO ()
main = do
  args <- getArgs
  result <- solve (head args) (head (tail args)) (tail (tail args))
  putStrLn result
  where

    solve :: String -> String -> [String] -> IO String
    solve "stronghold" "dna" = Rosalind.Stronghold.DNA.solve
    solve "stronghold" "rna" = Rosalind.Stronghold.RNA.solve
