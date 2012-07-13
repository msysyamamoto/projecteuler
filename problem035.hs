import Euler
main = putStrLn $ show solve
solve = length [x | x <- subjects, all isPrime (circular x)]

subjects :: [Integer]
subjects = filter check $ primes 1000000
  where
    check 2 = True
    check 5 = True
    check n = allElem (show n) "1379"

allElem :: Eq a => [a] -> [a] -> Bool
allElem as bs = and $ allElem' as bs
  where
    allElem' [] _ = [True]
    allElem' (x:xs) ys = (x `elem` ys) : allElem' xs ys

circular :: Integer -> [Integer] 
circular n = map read $ cyclic $ show n

cyclic :: [a] -> [[a]]
cyclic []     = []
cyclic xs = take (length xs) (xs : cyclic' xs)
  where
    cyclic' [] = error "bad param" 
    cyclic' (a:as) = let as' = as ++ [a]
                      in as' : cyclic' as'

isPrime :: Integral a => a -> Bool
isPrime n
  | n <= 1    = False
  | otherwise = not $ any (\x -> n `mod` x == 0) [2..(n-1)]
