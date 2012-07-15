import Data.Ord
import Data.List
main :: IO ()
main = print solve

solve :: Int
solve = fst $ pcountMax 1000 

pcountMax :: Int -> (Int , Int) -- (p, count)
pcountMax maxp = maximumBy (comparing snd) $ zip [1..] (map pcount [1..maxp])

pcount :: Int -> Int
pcount p = sum [1| a <- [1..p], b <- [a..p], c <- [p - a - b], c^2 == a^2 + b^2]
