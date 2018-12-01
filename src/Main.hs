module Main where
import System.Environment (getArgs)
import qualified Rosalind.Stronghold.DNA
import qualified Rosalind.Stronghold.RNA
import qualified Rosalind.Stronghold.REVC
import qualified Rosalind.Stronghold.FIB
import qualified Rosalind.Stronghold.GC


main :: IO ()
main = do
  args <- getArgs
  result <- solve args
  putStrLn result
  where

    solve :: [String] -> IO String
    solve (section:problem:args) = solve' section problem args

    solve' "stronghold" "dna"  = Rosalind.Stronghold.DNA.solve
    solve' "stronghold" "rna"  = Rosalind.Stronghold.RNA.solve
    solve' "stronghold" "revc" = Rosalind.Stronghold.REVC.solve
    solve' "stronghold" "fib"  = Rosalind.Stronghold.FIB.solve
    solve' "stronghold" "gc"   = Rosalind.Stronghold.GC.solve
