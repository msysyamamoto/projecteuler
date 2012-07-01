module Euler
( factors
, factorization
, factors2
) where

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

factors2 n = 1 : map product combination
  where
    combination = tail $ subs $ factorization n 

subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
  where yss = subs xs
