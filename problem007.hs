solve = last $ take 10001 primes 

primes :: [Int]
primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (p:xs) = p : sieve [x| x <- xs, x `mod` p /= 0]
