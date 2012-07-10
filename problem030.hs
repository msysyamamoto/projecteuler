solve :: Int
solve = sum [n | n <- [2..999999], sumDigit n == n]

sumDigit :: Int -> Int
sumDigit n = sum $ map fact5 $ show n

fact5 :: Char -> Int
fact5 '0' = 0
fact5 '1' = 1
fact5 '2' = 32
fact5 '3' = 243
fact5 '4' = 1024
fact5 '5' = 3125
fact5 '6' = 7776
fact5 '7' = 16807
fact5 '8' = 32768
fact5 '9' = 59049
fact5 _   = error "invalid charcter"
