module Main where

import Data.List

factorial :: (Eq a, Num a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

numberOfPermutedPalins :: Ord a => [a] -> Integer
numberOfPermutedPalins xs = numer `div` denom
  where freqs = map (toInteger . length) $ group $ sort xs
        numer = factorial $ sum $ map (`div` 2) freqs
        denom = product $ map (factorial . (`div` 2)) freqs

main :: IO ()
main = do
    input <- getLine
    print $ numberOfPermutedPalins input `mod` (10^9 + 7)
