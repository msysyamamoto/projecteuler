import Data.Char

main = do
  line <- getLine
  print $ solve line

solve :: String -> Int 
solve line = sum [1| s <- subjects, isTriangleWord s]
  where
    subjects = words $ beforeWords line  

beforeWords :: String -> String
beforeWords = foldr replace []
  where
    replace '"' acc = acc
    replace ',' acc = ' ' : acc
    replace ch  acc = ch  : acc

triangleNumbers :: [Int] 
triangleNumbers = foldr (\n acc -> trinum n : acc) [] [1..]

trinum :: Int -> Int 
trinum n = (n * (n + 1)) `div` 2

alphaToNum :: Char -> Int
alphaToNum alpha
  | alpha `elem` ['A'..'Z'] = ord alpha - ord 'A' + 1
  | otherwise               = 0

toNum :: String -> Int
toNum str = sum [alphaToNum x | x <- str]

isTriangleWord :: String -> Bool
isTriangleWord = isTriangleNumber . toNum

isTriangleNumber :: Int -> Bool
isTriangleNumber num = last smaller == num
  where
    smaller = takeWhile (<= num) triangleNumbers
