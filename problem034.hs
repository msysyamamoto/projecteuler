import Data.Char
import Euler

main = putStrLn $ show solve

solve :: Int
solve = sum nums

nums :: [Int]
nums = foldr test [] [3..999999]
  where
    test n acc
      | sum (map factorial (toDigits n)) == n = n:acc
      | otherwise = acc

toDigits :: Int -> [Int]
toDigits = map digitToInt . show
