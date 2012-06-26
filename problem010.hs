primeList :: Int -> [Integer]
primeList n = map fromIntegral $ 2 : sieve [3, 5 .. n]
    where
      isqrt = (truncate . sqrt . fromIntegral) n
      sieve (p : xs)
          | p > isqrt = p : xs
          | otherwise = p : sieve [x | x <- xs, rem x p /= 0]
 
main = print $ sum $ primeList 2000000
