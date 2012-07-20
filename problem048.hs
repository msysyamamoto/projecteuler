solve :: Integer
solve = (sum $ map (\n -> n^n) [1..1000]) `mod` 10000000000
