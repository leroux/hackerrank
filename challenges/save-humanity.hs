module Main where

import Control.Monad

matches :: Eq a => [a] -> [a] -> Bool
matches = almostMatches 0

almostMatches :: Eq a => Int -> [a] -> [a] -> Bool
almostMatches i [] [] = i <= 1
almostMatches _ [] _ = False
almostMatches _ _ [] = False
almostMatches i (x:xs) (y:ys)
  | i > 1 = False
  | x == y = almostMatches i xs ys
  | otherwise = almostMatches (i + 1) xs ys

almostIsPrefix :: Eq a => Int -> [a] -> [a] -> Bool
almostIsPrefix i [] _ = i <= 1
almostIsPrefix _ _ [] = False
almostIsPrefix i (x:xs) (y:ys)
  | i > 1 = False
  | x == y = almostIsPrefix i xs ys
  | otherwise = almostIsPrefix (i + 1) xs ys

substringsOfLength :: Int -> [a] -> [(Int, [a])]
substringsOfLength = substringsOfLength' 0

substringsOfLength' :: Int -> Int -> [a] -> [(Int, [a])]
substringsOfLength' _ _ [] = []
substringsOfLength' i k xs = (i, substring) : substringsOfLength' (i + 1) k rest 
  where substring = take k xs
        rest = drop 1 xs

matchesSubstring :: Eq a => [a] -> (t, [a]) -> Bool
matchesSubstring xs (_, s) = matches xs s

findMatches :: Eq a => [a] -> [a] -> [(Int, [a])]
findMatches xs ys = filter matchesWith generatedSubstrings
  where matchesWith = matchesSubstring xs
        generatedSubstrings = substringsOfLength (length xs) ys

matchedIndices :: Eq a => [a] -> [a] -> [Int]
matchedIndices = matchedIndices' 0

matchedIndices' :: (Eq a1, Num a) => a -> [a1] -> [a1] -> [a]
matchedIndices' _ [] _ = []
matchedIndices' i p v 
  | v `matches` curr = i : matchedIndices' (i + 1) (drop 1 p) v
  | otherwise = matchedIndices' (i + 1) (drop 1 p) v
  where curr = take (length v) p

matchingIndices :: Eq a => [a] -> [a] -> [Int]
matchingIndices ys xs = map fst $ findMatches xs ys

uglyShowList :: Show a => [a] -> String
uglyShowList [] = ""
uglyShowList (x:xs) = show x ++ " " ++ uglyShowList xs

main :: IO ()
main = do
    tStr <- getLine
    let t = read tStr :: Int
    p' <- getLine
    v' <- getLine
    putStrLn $ uglyShowList $ matchedIndices p' v'
    forM_ [2..t] $ \_ -> do
        _ <- getLine
        p <- getLine
        v <- getLine
        putStrLn $ uglyShowList $ matchedIndices p v
