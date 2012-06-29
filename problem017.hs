solve = thousand + hundred 
  where
    thousand = length "onethousand"
    hundred  = length $ concat [numWords x| x <-[1..999]]

numWords2 :: Int -> String
numWords2 n
  | d2 <= 0   = ""
  | d2 <= 19  = inWords d2 
  | otherwise = inWords d2 ++ inWords (n `mod` 10) 
  where
    d2 = n `mod` 100

numWords3 :: Int -> String
numWords3 n
  | d3 <= 0   = ""
  | otherwise = inWords d3 ++ "hundred" 
  where
    d3 = n `div` 100

numWords :: Int -> String
numWords n
  | null d3 = d2
  | null d2 = d3
  | otherwise = d3 ++ "and" ++ d2
  where
    d2 = numWords2 n
    d3 = numWords3 n

inWords :: Int -> String
inWords 0  = ""
inWords 1  = "one"
inWords 2  = "two"
inWords 3  = "three"
inWords 4  = "four"
inWords 5  = "five"
inWords 6  = "six"
inWords 7  = "seven"
inWords 8  = "eight"
inWords 9  = "nine"
inWords 10 = "ten"
inWords 11 = "eveven"
inWords 12 = "twelve"
inWords 13 = "thirteen"
inWords 14 = "fourteen"
inWords 15 = "fifteen"
inWords 16 = "sixteen"
inWords 17 = "seventeen"
inWords 18 = "eighteen"
inWords 19 = "nineteen"
inWords n
  | n < 30    = "twenty"
  | n < 40    = "thirty"
  | n < 50    = "forty"
  | n < 60    = "fifty"
  | n < 70    = "sixty"
  | n < 80    = "seventy"
  | n < 90    = "eighty"
  | otherwise = "ninety"
