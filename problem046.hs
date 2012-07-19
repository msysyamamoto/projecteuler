import Euler
import Control.Monad

main = print $ take 1 solve

solve :: [Integer]
solve = do
  o <- oddCompositeNumbers
  guard $ check o $ primes $ fromIntegral o
  return o

oddCompositeNumbers :: [Integer]
oddCompositeNumbers = filter test [9, 11 ..]
  where
    test n = not $ millerRabin n (n + 3) 

check :: Integer -> [Integer] -> Bool
check _ [] = True
check oc (p:ps)
  | d `elem` (takeWhile (<= d) twiceSquares) = False
  | otherwise = check oc ps
  where
    d = oc - p

twiceSquares :: [Integer]
twiceSquares = map (\n -> 2 * (n^2)) [1..] 
