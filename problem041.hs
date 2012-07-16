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
    subjects' [n]    = (reverse $ sort $ map read $ permutations $ map intToDigit [1..n]) 
    subjects' (n:ns) = (reverse $ sort $ map read $ permutations $ map intToDigit [1..n]) ++ subjects' ns

isPrime :: Integer -> Bool
isPrime n = millerRabin n (n - 2)
