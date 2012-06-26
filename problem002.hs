solve :: Int
solve = sum [x| x <- filter even (takeWhile (< limit) fib)]

limit :: Int
limit = 4000000

fib :: [Int]
fib = 1:2:zipWith (+) fib (tail fib)
