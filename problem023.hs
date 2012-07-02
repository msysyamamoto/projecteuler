import Euler

main = putStrLn $ show solve
solve = sum $ filter cantBeWritten [1..28123]

cantBeWritten :: Int -> Bool
cantBeWritten n = and [not $ isAbundant (n - x) | x <- takeWhile (<= div n 2) abundants]
  
abundants :: [Int]
abundants = [x| x <- [12..28123], isAbundant x]

isAbundant :: Int -> Bool
isAbundant n = n < sum (properDivisors n)
