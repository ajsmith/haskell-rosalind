module Main where
import System.Environment (getArgs)
import qualified Rosalind.Stronghold.DNA
import qualified Rosalind.Stronghold.RNA
import qualified Rosalind.Stronghold.REVC
import qualified Rosalind.Stronghold.FIB
import qualified Rosalind.Stronghold.GC
import qualified Rosalind.Stronghold.HAMM
import qualified Rosalind.Stronghold.IPRB
import qualified Rosalind.Stronghold.PROT
import qualified Rosalind.Stronghold.SUBS
import qualified Rosalind.Stronghold.CONS


main :: IO ()
main = do
  args <- getArgs
  result <- solve args
  putStrLn result
  where

    solve :: [String] -> IO String
    solve (section:problem:args) = solve' section problem args

    solve' :: String -> String -> [String] -> IO String
    solve' "stronghold" "dna"  = Rosalind.Stronghold.DNA.solve
    solve' "stronghold" "rna"  = Rosalind.Stronghold.RNA.solve
    solve' "stronghold" "revc" = Rosalind.Stronghold.REVC.solve
    solve' "stronghold" "fib"  = Rosalind.Stronghold.FIB.solve
    solve' "stronghold" "gc"   = Rosalind.Stronghold.GC.solve
    solve' "stronghold" "hamm" = Rosalind.Stronghold.HAMM.solve
    solve' "stronghold" "iprb" = Rosalind.Stronghold.IPRB.solve
    solve' "stronghold" "prot" = Rosalind.Stronghold.PROT.solve
    solve' "stronghold" "subs" = Rosalind.Stronghold.SUBS.solve
    solve' "stronghold" "cons" = Rosalind.Stronghold.CONS.solve
