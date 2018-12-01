module Rosalind.Stronghold.IPRB where


{- Mendelian probabilities for dominant alleles

k: number of homozygous dominant individuals
m: number of heterozygous individuals
n: number of homozygous recessive individuals

To produce an offspring with a dominant allele, there must be a
pairing of:

- One homozygous dominant with another homozygous dominant.
  P(k / total * (k - 1) / (total - 1))

- One homozygous dominant with a heterozygous
  P(k / total * m / (total - 1))

- One homozygous dominant with a homozygous recessive
  P(k / total * n / (total - 1))

- One heterozygous with a homozygous dominant (duplicate of above)

- One heterozygous with another heterozygous
  P(m / total * (m - 1) / (total - 1) * 3 / 4)

- One heterozygous with a homozygous recessive
  P(m / total * n / (total - 1) / 2)

- One homozygous recessive with a homozygous dominant (duplicate of above)

- One homozygous recessive with a heterozygous (duplicate of above)

- Homozygous recessive with homozygous recessive cannot produce
  dominant alleles.

-}
probabilityDominant :: (Integral a, Fractional b) => a -> a -> a -> b
probabilityDominant k m n = ratio (sum probs) (4 * q)
  where
    total = k + m + n
    q = total^2 - total
    ratio a b = (fromIntegral a) / (fromIntegral b)
    -- p1 is homozygous dominant with homozygous dominant. occurs once.
    p1 = 4 * (k^2 - k)
    -- p2 is homozygous dominant with heterozygous. occurs twice.
    p2 = 2 * 4 * k * m
    -- p3 is homozygous dominant with homozygous recessive. occurs twice.
    p3 = 2 * 4 * k * n
    -- p4 is heterozygous with heterozygous. occurs once.
    p4 = 3 * (m^2 - m)
    -- p5 is heterozygous with homozygous recessive. occurs twice.
    p5 = 2 * 2 * m * n
    probs = [p1, p2, p3, p4, p5]

parse :: String -> [Int]
parse raw = [read w :: Int | w<-(words line)]
  where
    line = head [l | l<-(lines raw), l /= ""]

solve :: [String] -> IO String
solve args = do
  source <- readFile (head args)
  let (k:m:n:[]) = parse source
  return $ show $ probabilityDominant k m n
