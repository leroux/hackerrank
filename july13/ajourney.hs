module Main where
  
import Control.Monad

takeLast :: Int -> [a] -> [a]
takeLast k = reverse . take k . reverse

-- numberOfWays :: Integer -> Integer
-- numberOfWays 0 = 0
-- numberOfWays n = 1 + sum (map numberOfWays [1..(n-1)])

numberOfWays :: Int -> Integer
numberOfWays n = 2 ^ (n - 1)

sumFirstLast :: Int -> Integer -> Int
sumFirstLast k s = read (take k sStr) + read (takeLast k sStr)
  where sStr = show s

numberOfWaysInKForm :: Int -> Int -> Int
numberOfWaysInKForm n k = sumFirstLast k (numberOfWays n)

findNumWays :: String -> String
findNumWays line = show $ numberOfWaysInKForm n k
  where args = map read (words line) :: [Int]
        n = head args
        k = args !! 1

main :: IO ()
main = do
    tStr <- getLine
    let t = read tStr :: Int
    forM_ [1..t] $ \_ -> do
        input <- getLine
        putStrLn $ findNumWays input
