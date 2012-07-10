solve :: Integer
solve = sum [sumCorners n| n <- [3, 5 .. 1001]] + 1

sumCorners :: Integer -> Integer
sumCorners n = upperRight + upperLeft + lowerLeft + lowerRight
  where
    n' = n - 1
    upperRight = n ^ 2
    upperLeft  = upperRight - n'
    lowerLeft  = upperLeft  - n'
    lowerRight = lowerLeft  - n'
