import Control.Monad
import Data.List
import Euler

solve = sumpair $ odds nums 
  where
    nums = takeWhile (\(a, b) -> a <= 10000 && b <= 10000) amicableNumbers

properDivisors :: Integral a => a -> [a]
properDivisors n = init $ factors n

amicableNumbers :: Integral a => [(a, a)]
amicableNumbers = do
  x <- [220..]
  let suma = sum (properDivisors x)
      sumb = sum (properDivisors suma)
  guard (x /= suma && sumb == x)
  return (x, suma)

odds :: [a] -> [a]
odds xs = map snd $ filter (\(i, _) -> odd i) $ zip [1..] xs

sumpair :: Num a => [(a, a)] -> a
sumpair = foldl' (\acc (a, b) -> a + b + acc) 0
