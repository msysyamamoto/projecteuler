solve = take 1 $ dropWhile (\(_, n) -> n <= border) $ zip [1..] fib

fib :: [Integer]
fib = 1:1:zipWith (+) fib (tail fib)

border :: Integer
border = 10^999 - 1
