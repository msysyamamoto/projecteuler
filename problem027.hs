import Euler
import Data.Ord
import Data.List

main :: IO ()
main = putStrLn $ show solve

solve :: (Int, (Integer, Integer))
solve = maximumBy (comparing fst) lengthAndAB

ps :: [Integer]
ps = primes 10000

isPrime :: Integer -> Bool
isPrime n = n `elem` ps 

expression :: Integer -> Integer -> Integer -> Integer
expression a b = \n -> n^2 + a * n + b

candidateA :: [Integer]
candidateA = [-999..999] 

candidateB :: [Integer]
candidateB = primes' ++ (map negate primes')
  where
    primes' = primes 1000

lengthAndAB :: [(Int, (Integer, Integer))]
lengthAndAB = do
  a  <- candidateA
  b  <- candidateB
  let ns = takeWhile isPrime $ map (expression a b) [0..1601]
  return (length ns, (a, b))
