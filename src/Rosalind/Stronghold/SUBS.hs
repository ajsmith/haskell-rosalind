module Rosalind.Stronghold.SUBS where

motifs :: String -> String -> [Int]
motifs s t = motifs' (chunks s t)
  where
    motifs' :: [String] -> [Int]
    motifs' ws = [fst e | e<-(zip [1..(length ws)] ws), snd e == t]

    chunks :: String -> String -> [String]
    chunks s t =
      if length s < length t
      then []
      else (take (length t) s):(chunks (tail s) t)

parse :: String -> [String]
parse raw = take 2 $ [l | l <-(lines raw), l /= ""]

solve :: [String] -> IO String
solve args = do
  source <- readFile (head args)
  let
    (s:t:[]) = parse source
    result = unwords $ map show $ motifs s t
  return result
