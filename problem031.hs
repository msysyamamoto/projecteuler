{-
http://parametron.blogspot.jp/2010/01/blog-post_24.html
-}

solve :: Integer
solve = cc coins 200

coins :: [Integer]
coins = [200, 100, 50, 20, 10, 5, 2, 1]

cc :: [Integer] -> Integer -> Integer
cc [] _ = 0
cc (d:ds) a
  | a == 0 = 1
  | a <= 0 = 0
  | otherwise = cc (d:ds) (a - d) + cc ds a
