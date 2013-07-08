module Main where

import Data.List

main :: IO ()
main = do
    input <- getLine
    let friend = (map read $ words input :: [Int]) !! 1
    unfriendsStr <- getLine
    let unfriends = map read $ words unfriendsStr :: [Int]
    print $ length $ unfriendlies friend unfriends
    return ()

divisibleBy :: Int -> Int -> Bool
n `divisibleBy` x = n `mod` x == 0

notDivisibleBy :: Int -> Int -> Bool
n `notDivisibleBy` x = not $ n `divisibleBy` x

dividesAll :: Int -> [Int] -> Bool
dividesAll x = all (`divisibleBy` x)

divisors :: Int -> [Int]
divisors n = [x | x <- [1..n], n `divisibleBy` x]

unfriendlies :: Int -> [Int] -> [Int]
unfriendlies friend unfriends =
    nub [x | x <- divisors friend, x `dividesAll` unfriends]
