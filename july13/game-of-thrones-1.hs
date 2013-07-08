module Main where

import Data.List

occurrences :: Eq a => a -> [a] -> Int
occurrences n xs = occurrences' n xs 0
  where occurrences' _ [] i = i
        occurrences' n' (x:xs') i
          | n' == x = occurrences' n' xs' (i + 1)
          | otherwise = occurrences' n' xs' i

onlyOne :: (a -> Bool) -> [a] -> Bool
onlyOne p xs = onlyOne' p xs 0
  where onlyOne' _ [] i = i == 1
        onlyOne' p' (x:xs') i
          | i > 1 = False
          | p x = onlyOne' p' xs' (i + 1)
          | otherwise = onlyOne' p' xs' i

possiblePalindrome :: Eq a => [a] -> Bool
possiblePalindrome xs
  | (even . length) xs = evenOccurrences xs
  | otherwise = oneOddOccurenceOnly xs
  where evenOccurrences xs' = all even $ map (`occurrences` xs') (nub xs')
        oneOddOccurenceOnly xs' = onlyOne odd $ map (`occurrences` xs) (nub xs')

main :: IO ()
main = do
    input <- getLine
    putStrLn $ if possiblePalindrome input then "YES" else "NO"
