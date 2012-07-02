module Euler
( factors
, divisors
, factorization
, properDivisors 
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
factors :: Integral a => a -> [a]
factors n = [x | x <- [1..n], n `mod` x == 0]
-- alias
divisors :: Integral a => a -> [a]
divisors num = sort $ divisors' num [1..num]
  where
    divisors' _ [] = []
    divisors' n (x:xs)
      | n < x ^ 2       = []
      | n == x * x    = [x]
      | n `mod` x == 0  = x : (n `div` x) : divisors' n xs
      | otherwise       = divisors' n xs

-- 真の約数
-- ex properDivisors 10 => [1,2,5]
properDivisors :: Integral a => a -> [a]
properDivisors n = filter (/= n) $ divisors n
