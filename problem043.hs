import Data.Char
import Data.List
import Control.Monad

solve = sum pandigitals

-- p で割り切れる 3 桁の数の配列を作成する
-- 12 等 2 桁の数も 012 として 3 桁として扱う 
triple :: Int -> [Int]
triple p = filter (not . hasSameDigit) $ triple' [1..]
  where
    triple' [] = error "invalid param." 
    triple' (n:ns)
      | p * n < 10   = triple' ns 
      | p * n < 1000 = p * n : triple' ns
      | otherwise    = []

-- 同じ数が使われているか判定
hasSameDigit :: Int -> Bool
hasSameDigit num = nub numstr /= numstr
  where
    numstr = show num 

pandigitals :: [Integer]
pandigitals = do
  div17 <- triple 17
  div13 <- filter (test div17) $ triple 13
  div11 <- filter (test div13) $ triple 11
  div7  <- filter (test div11) $ triple 7
  div5  <- filter (test div7)  $ triple 5
  div3  <- filter (test div5)  $ triple 3
  div2  <- filter (test div3)  $ triple 2
  let d1 = restNumber [div2, div3, div5, div7, div11, div13, div17]
  guard $ length d1 == 1
  return $ read $ show (head d1) ++ show div2 ++ format [div3, div5, div7, div11, div13, div17] 

-- dn の上位2桁と dm の下位 2 桁がひとしいか判定
test :: Int -> Int -> Bool
test dn dm = dn `div` 10 == dm `mod` 100

format :: [Int] -> String
format = foldr (\x acc -> last (show x) : acc) []

restNumber :: [Int] -> [Int]
restNumber xs = [digitToInt n | n <- ['0'..'9'], not (n `elem` used)]
  where
    used = nub $ concatMap show xs
