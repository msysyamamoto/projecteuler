{-
http://ja.wikipedia.org/wiki/%E3%83%95%E3%82%A7%E3%83%AB%E3%83%9E%E3%83%BC%E3%81%AE%E5%B0%8F%E5%AE%9A%E7%90%86
http://ja.wikipedia.org/wiki/%E3%82%AA%E3%82%A4%E3%83%A9%E3%83%BC%E3%81%AE%E5%AE%9A%E7%90%86_(%E6%95%B0%E8%AB%96)
-}
 
import Data.List
import Data.Ord

solve = maximumBy (comparing snd) [(x, recurringLength x)| x <- items, gcd x 10 == 1] 
  where items = [2..999]

recurringLength :: Int -> Int
recurringLength m = 1 + (length (recurringCycle m))

recurringCycle :: Int -> [Int]
recurringCycle m = takeWhile (/= 1) $ calc 1 m

calc :: Int -> Int -> [Int]
calc n m = mod' : calc mod' m
  where mod' = (n * 10) `mod` m
