solve = route 20 20

route :: Integral a => a -> a -> a
route n m = factorial (n + m) `div` (factorial n * factorial m)

factorial :: Integral a => a -> a
factorial n = product [1..n]
