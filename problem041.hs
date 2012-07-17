import Euler
import Data.Char
import Data.List

main = print solve

solve :: Integer
solve = head [x | x <- subjects, isPrime x]

subjects :: [Integer]
subjects = subjects' [9,8..1]
  where
    subjects' []     = [] 
    subjects' [n]    = prems n 
    subjects' (n:ns) = prems n ++ subjects' ns
    prems n = reverse $ sort $ map read $ permutations $ map intToDigit [1..n]

isPrime :: Integer -> Bool
isPrime n = millerRabin n (n - 2)
