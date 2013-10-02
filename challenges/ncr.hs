module Main where

import Control.Monad

choose n 0 = 1
choose 0 k = 0
choose n k = choose (n-1) (k-1) * n `div` k 

main :: IO ()
main = do
    tStr <- getLine
    let t = read tStr :: Int
    forM_ [1..t] $ \_ -> do
        args <- getLine
        let parsed = map read (words args) :: [Integer]
        print $ choose (parsed !! 0) (parsed !! 1) `mod` 142857
