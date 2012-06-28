solve = head $ filter (\n -> factorCount n >= 501) (map tris [1..])

factorCount :: Integer -> Integer
factorCount n = product $ map ((+1) . snd) $ foldr count [] (factorization n)
  where
    count x [] = [(x, 1)]
    count x ((m, c):ms)
      | m == x    = (m, c + 1) : ms
      | otherwise = (x, 1) : (m, c) : ms

factorization :: Integer -> [Integer]
factorization 1 = [] 
factorization n  = m : factorization (n `div` m)
  where
    m = (factor n) !! 1

factor :: Integer -> [Integer]
factor n = [x | x <- [1..n], n `mod` x == 0]

tris :: Integer -> Integer
tris n = sum [1..n]
