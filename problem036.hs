solve :: Int
solve = sum palindromes

palindromes :: [Int]
palindromes = filter (isPalindrome . toBinary) subject

subject :: [Int]
subject = filter (isPalindrome . show) [1..(1000000 - 1)]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

toBinary :: Integral a => a -> [a]
toBinary 0 = []
toBinary n = toBinary (n `div` 2) ++ [n `mod` 2]
