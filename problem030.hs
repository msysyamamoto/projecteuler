import Data.Char

solve = sum [n | n <- [2..999999], sumDigit n == n]

sumDigit :: Int -> Int
sumDigit n = sum $ map (^5) $ map digitToInt $ show n


