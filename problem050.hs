import Euler
import Data.List
import Data.Ord

main = print $ snd solve

solve :: (Int, Integer)
solve = maximumBy (comparing fst) $ earchStart items 

items :: [Integer]
items = primes $ fromIntegral limit 

limit :: Integer
limit = 1000000

earchStart :: [Integer] -> [(Int, Integer)]
earchStart [] = []
earchStart ps@(p0:pn)
  | p0 <= limit `div` 2 = calc ps 1 0 (0, 0) : earchStart pn
  | otherwise           = []


calc :: [Integer] -> Int -> Integer -> (Int, Integer) -> (Int, Integer)
calc [] _ _ m = m
calc (p:ps) i acc m 
  | acc' > limit       = m
  | millerRabin acc' p = calc ps (i + 1) acc' (i, acc') 
  | otherwise          = calc ps (i + 1) acc' m
  where
    acc' = p + acc
