import Data.List
import Control.Monad

main = putStrLn $ show solve

solve = sum $ nub candidates

candidates = concat $ filter (not . null) $ map test $ permutation [1..9]

test xs = do
  (as, bs) <- splits xs
  (cs, ds) <- splits bs 
  let a = toInt as
      c = toInt cs
      d = toInt ds
  guard $ a * c == d
  return d

permutation :: Eq a => [a] -> [[a]]
permutation [] = [[]]
permutation xs = concat [map (x:) $ permutation (delete x xs) | x <- xs]

splits :: [a] -> [([a], [a])]
splits items = splits' 1 items 
  where
    splits' n xs = let (hs, ts) = splitAt n xs
                    in if null ts
                       then []
                       else (hs, ts) : splits' (n + 1) xs

toInt :: [Int] -> Int
toInt = foldl' (\acc x -> acc * 10 + x) 0
