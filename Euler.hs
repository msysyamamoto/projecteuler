module Euler
( factors
, divisors
, factorization
, properDivisors 
, primes
, factorial 
, permutation
, millerRabinTest
, millerRabin
) where

import Data.List
import System.Random
import Control.Applicative
import Control.Monad

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


-- 素数判定: ミラーラビン法
millerRabinTest :: Int -> Integer -> IO Bool
millerRabinTest times n = and <$> replicateM times (millerRabinRandom n)

factor2 :: Integer -> (Int, Integer)
factor2 n = factor2' (0,n)
  where
    factor2' (s,d)
      | even d    = factor2' (s + 1, d `div` 2)
      | otherwise = (s,d)

millerRabin :: Integer -> Integer -> Bool
millerRabin n a = 
  let (s,d) = factor2 (n - 1)
      aExpD = expmod a d n
      double x = expmod x 2 n
      evens = take s $ iterate double aExpD
  in aExpD == 1 || any (== n - 1) evens

millerRabinRandom :: Integer -> IO Bool
millerRabinRandom n =  millerRabin n <$> getRandom 2 (n - 2)

expmod :: Integer -> Integer -> Integer -> Integer
expmod base ex m
  | ex == 0   = 1
  | even ex   = square (expmod base (ex `div` 2) m) `mod` m
  | otherwise = (base * (expmod base (ex - 1) m)) `mod` m
 where
   square x = x * x

getRandom n m = getStdRandom $ randomR (n, m)
