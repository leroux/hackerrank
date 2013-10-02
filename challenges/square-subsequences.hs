module Main where

import Control.Monad
import Data.List
import qualified Data.Map as M

--- Compute the frequencies of each distinct element in a list.
frequency :: Ord a => [a] -> [Int] 
-- frequency xs = map length (group (sort xs))
frequency xs = map snd $ M.toList $ M.fromListWith (+) [(c, 1) | c <- xs] 

--- Split a list into exactly two halves of almost/exactly the same length.
halves :: [a] -> ([a], [a])
halves [] = ([], [])
halves xs
  | (even . length) xs = (take n xs, drop n xs)
  | otherwise = (take (n+1) xs,  drop (n+1) xs)
  where n = length xs `div` 2 

--- Check if a list is a square list.
-- A string is called a square string if it can be obtained by
-- concatenating two copies of the same string.
-- Therefore if the first half of the list is the same as the second half
-- it is a square list.
isSquareList :: (Eq a, Ord a) => [a] -> Bool
isSquareList [] = False
isSquareList xs
  | (odd . length) xs = False
  | hasOddFreq = False
  | otherwise = as == bs
  where (as, bs) = halves xs
        hasOddFreq = any odd $ frequency xs

--- Compute the number of square lists in the subsequences of a list.
squaresInSubsequences :: (Eq a, Ord a) => [a] -> Integer
squaresInSubsequences = genericLength . filter isSquareList . subsequences . removeSingleOccurs

--- Remove all single occurence elements from a list.
removeSingleOccurs :: (Eq a, Ord a) => [a] -> [a]
removeSingleOccurs xs = xs \\ occursOnce xs
  where occursOnce = concat . filter (\l -> length l == 1) . group . sort

main :: IO ()
main = do
    tStr <- getLine
    let t = read tStr :: Int
    forM_ [1..t] $ \_ -> do
        s <- getLine
        print $ compute s
  where compute s' = squaresInSubsequences s' `mod` 1000000007
