solve :: Integer
solve = (same hexagonals pentagonals triangles) !! 2 -- [1, 40755, answer ...]

same :: [Integer] -> [Integer] -> [Integer] -> [Integer]
same [] _ _ = []
same (h:hs) ps ts
  | h == head ps' && h == head ts' = h : same hs ps' ts'
  | otherwise                      = same hs ps' ts'
  where
    ps' = dropWhile (< h) ps
    ts' = dropWhile (< h) ts

triangle :: Integral a => a -> a
triangle n = (n * (n + 1)) `div` 2

triangles :: Integral a => [a]
triangles = polygonalNumbers triangle 

pentagonal :: Integral a => a -> a
pentagonal n = (n * (3 * n - 1)) `div` 2

pentagonals :: Integral a => [a]
pentagonals = polygonalNumbers pentagonal

hexagonal :: Integral a => a -> a
hexagonal n = n * (2 * n - 1)

hexagonals :: Integral a => [a]
hexagonals = polygonalNumbers hexagonal

polygonalNumbers :: Integral a => (a -> a) -> [a]
polygonalNumbers f =  map f [1..]
