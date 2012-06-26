import Data.List

solve :: Integer -> Integer
solve n = maximum $ primes n

primes :: Integer -> [Integer]
primes num = primes' num 2 []
  where
    limit = round $ sqrt $ fromIntegral num
    primes' n m acc
      | m > limit = nub acc
      | mod n m == 0 = primes' (n `div` m) m (m:acc) 
      | otherwise    = primes' n (m + 1) acc
