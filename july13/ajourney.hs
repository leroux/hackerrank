module Main where
  
import Control.Monad

takeLast :: Int -> [a] -> [a]
takeLast k = reverse . take k . reverse

lengthOfInteger :: Integer -> Int
lengthOfInteger 0 = 0
lengthOfInteger x = 1 + lengthOfInteger (x `div` 10)

takeFromInteger :: Int -> Integer -> Integer
takeFromInteger k x = x `div` (10 ^ (l - k))
  where l = lengthOfInteger x

takeLastFromInteger :: Int -> Integer -> Integer
takeLastFromInteger k x = x `mod` (10 ^ k)

{-
numberOfWays' 1 = 1
numberOfWays' n = 1 + (sum $ map numberOfWays [1..(n - 1)])
-}

numberOfWays :: Int -> Integer
numberOfWays n = 2 ^ (n - 1)

sumFirstLast :: Int -> Integer -> Integer
sumFirstLast k s = takeFromInteger k s + takeLastFromInteger k s

numberOfWaysInKForm :: Int -> Int -> Integer
numberOfWaysInKForm n k = sumFirstLast k s
  where s = numberOfWays n

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
