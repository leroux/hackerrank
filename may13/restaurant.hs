module Main where
  
import Control.Monad

main :: IO ()
main = do
    tStr <- getLine
    let t = read tStr :: Int
    forM_ [1..t] $ \_ -> do
        line <- getLine
        let nums = map read (words line) :: [Int]
        let l = head nums
            b = nums !! 1
        print $ maxSquares l b
        return ()
    return ()
      
maxSquares :: Integral t => t -> t -> t
maxSquares l b = last [(l `div` x) * (b `div` x) | x <- [1 .. min l b], l `mod` x == 0, b `mod` x == 0]
