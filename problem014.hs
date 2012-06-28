import Data.Ord
import Data.List

solve = maximumBy (comparing snd) [(x, collatz x 1) | x <- [1..999999]]

collatz :: Int -> Int -> Int
collatz 1 acc = acc
collatz n acc
  | even n    = collatz (n `div` 2) (acc + 1)
  | otherwise = collatz (3 * n + 1) (acc + 1)
