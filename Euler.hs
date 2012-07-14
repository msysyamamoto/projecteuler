module Euler
( factors
, divisors
, factorization
, properDivisors 
, primes
, factorial 
, permutations
) where

import Data.List

-- 素因数分解
-- ex. factorization 90 => [2,3,3,5]
factorization :: Integral a => a -> [a]
factorization 1 = [] 
factorization n  = m : factorization (n `div` m)
  where
    m = (factors n) !! 1

-- 約数
-- ex. factors 10 => [1,2,5,10]
divisors :: Integral a => a -> [a]
divisors num = sort $ divisors' num [1..num]
  where
    divisors' _ [] = []
    divisors' n (x:xs)
      | n < x ^ 2       = []
      | n == x * x    = [x]
      | n `mod` x == 0  = x : (n `div` x) : divisors' n xs
      | otherwise       = divisors' n xs

-- alias
factors :: Integral a => a -> [a]
factors n = divisors n 

-- 真の約数
-- ex properDivisors 10 => [1,2,5]
properDivisors :: Integral a => a -> [a]
properDivisors n = filter (/= n) $ divisors n


-- 素数のリスト
primes :: Int -> [Integer]
primes n = map fromIntegral $ 2 : sieve [3, 5 .. n]
    where
      isqrt = (truncate . sqrt . fromIntegral) n
      sieve (p : xs)
          | p > isqrt = p : xs
          | otherwise = p : sieve [x | x <- xs, rem x p /= 0]

-- 階乗
-- factorial 5 -> 120
factorial :: Integral a => a -> a
factorial n = product [1..n]

-- 順列
permutation :: Eq a => [a] -> [[a]]
permutation [] = [[]]
permutation xs = concat [map (x:) $ permutation (delete x xs) | x <- xs]
