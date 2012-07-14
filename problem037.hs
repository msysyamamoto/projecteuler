import Euler
main = putStrLn $ show solve

solve = sum $ take 11 $ filter test subjects 

-- 可能性のある素数を抽出
subjects :: [Integer]
subjects = filter check ps 
  where
    check n
      | (head nstr) `elem` "251379" = allElem (tail nstr) "1379" 
      | otherwise                   = False
      where
        nstr = show n

allElem :: Eq a => [a] -> [a] -> Bool
allElem as bs = and $ allElem' as bs
  where
    allElem' [] _ = [True]
    allElem' (x:xs) ys = (x `elem` ys) : allElem' xs ys

ps = primes 1000000

isPrime n = n `elem` ps

-- 左からの切り詰め
lchops num = lchops' 10 num
  where
    lchops' d n 
      | nmod == n = [n]
      | otherwise   = lchops' (d * 10) n ++ [nmod]
      where
        nmod = n `mod` d

-- 右からの切り詰め
rchops num
  | ndiv == 0   = [num] 
  | otherwise = num : rchops ndiv
  where
    ndiv = num `div` 10

-- 要件を満たすかチェック
test n
  | n <= 7    = False
  | otherwise = all isPrime (lchops n) && all isPrime (rchops n)
