module Main where
import System.Environment (getArgs)
import qualified Rosalind.Stronghold.DNA
import qualified Rosalind.Stronghold.RNA


main :: IO ()
main = do
  args <- getArgs
  putStrLn $ show $ solve (head args) (head (tail args)) (tail (tail args))
  where

    -- solution section problem args = putStrLn $ show $ solve section problem args
    -- putStrLn $ show $ Rosalind.Stronghold.DNA.solve 0 0 0 0 (head args)

    solve "stronghold" "dna" = Rosalind.Stronghold.DNA.solve
    solve "stronghold" "rna" = Rosalind.Stronghold.RNA.solve
