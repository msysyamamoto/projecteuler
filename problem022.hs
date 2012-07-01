import Data.Char
import Data.List

main = do
  line <- getLine
  let result = solve line
  putStrLn $ show result

solve :: String -> Int
solve line = sum $ zipWith score [1..] $ sort items
  where
    items = split line 

score :: Int -> String -> Int
score i name = i * nameScore name 

split :: String -> [String]
split str  = parseStart str []
  where
    parseStart ('"':ts) buf = parseItem ts [] buf
    parseItem (h:hs) item buf
      | h == '"'  = separator hs (buf ++ [item])
      | otherwise = parseItem hs (item ++ [h]) buf 
    separator (',':hs) buf = parseStart hs buf
    separator _ buf        = buf 

atoi :: Char -> Int
atoi ch = ord ch - 64 -- chr 'A' + 1 = 64

nameScore :: String -> Int 
nameScore = sum . map atoi
