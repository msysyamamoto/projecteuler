import Euler
import Data.List

solve :: String
solve = concatMap show $ head $ arithmeticSequence $ triples $ grouping fourDigitPrimes

-- 4桁の素数
fourDigitPrimes :: [Integer]
fourDigitPrimes = filter (> 1000) $ primes 9999

-- 数のリストの中から自分と同じ数字からなる数を探す
grouping :: [Integer] -> [[Integer]]
grouping subjects = [sameDigits s| s <- subjects]
  where
    sameDigits s = [p | p <- subjects, p > s, sort (show p) == sstr]
      where
        sstr = sort $ show s

-- 等差数列になっている数を探す
-- リストの要素数は 3 限定
arithmeticSequence :: (Num a, Eq a) => [[a]] -> [[a]]
arithmeticSequence = filter test 
  where
    test [a, b, c] = b - a == c - b
    test _         = error "Oops!!"

-- 自分と同じ数字からなる数が 3 つの数を探す
triples :: [[a]] -> [[a]]
triples = filter ((3 == ) . length)
