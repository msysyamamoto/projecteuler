{-
http://parametron.blogspot.jp/2010/01/blog-post_24.html
-}

solve :: Integer
solve = cc coins 200

coins :: [Integer]
coins = [200, 100, 50, 20, 10, 5, 2, 1]

cc :: [Integer] -> Integer -> Integer
cc [] _ = 0    -- (e) nが0なら, 両替の場合の数は0
cc ds@(dh:dt) a
  | a == 0    = 1 -- (c) aがちょうど0なら, 両替の場合の数は1
  | a < 0     = 0 -- (d) aが0より少なければ, 両替の場合の数は0
  | otherwise = cc dt a + cc ds (a - dh)
--               \           \_ (b) dを最初の硬貨の額面金額[denomination]として,
--                \                n種類の硬貨を使う, 金額a-dの両替の場合の数
--                 \
--                  \_ (a) 最初の種類の硬貨以外を使う,
--                         金額aの両替の場合の数, 足す
