import Euler
import Data.List
import Data.Char
import Control.Monad
main :: IO ()
main = putStrLn solve

solve :: String 
solve = map intToDigit $ head test

subjects :: [[Int]]
subjects = reverse $ sort $ permutations [1..9]

test :: [[Int]]
test = do
  s  <- subjects
  at <- [4, 3, 2, 1]
  n  <- [2..(9 `div` at)]
  guard $ build s at n == s
  return s

build :: [Int] -> Int -> Int -> [Int]
build s at n = map digitToInt $ concat $ map show $ zipWith (*) [1..n] $ repeat num 
  where
    num = toInt . fst $ splitAt at s

toInt :: [Int] -> Int
toInt = foldl' (\acc x -> acc * 10 + x) 0
