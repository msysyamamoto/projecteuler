import Data.Char

solve :: Int
solve = product $ map digitToInt $ map (dec !!) index

-- "012345..."
dec :: [Char] 
dec = ('0' :) $ concat $ map show [1..]

-- [1, 10, 100 ..]
index :: [Int]
index = take 7 $ iterate (10 *) 1
