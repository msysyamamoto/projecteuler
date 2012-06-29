data Weekday = Sun | Mon | Tue | Wed | Thu | Fri | Sat
  deriving (Show, Eq)

solve = length $ filter (\(d, w) -> d == 1 && w == Sun) $ drop1900 pairs 
  where
    pairs = zip daysOfCentury weekdays -- (日付, 曜日) のリストを 1900/1/1 から 2000/12/31 の期間分作る
    drop1900 = drop 365

daysOfCentury :: [Int]
daysOfCentury = foldr (\year acc -> daysOfYear year ++ acc) [] [1900..2000]

daysOfYear :: Int -> [Int]
daysOfYear year
  | isLeap year = concatMap days daysLeap  
  | otherwise   = concatMap days daysCommon

days :: Int -> [Int]
days n = [1..n]

weekdays :: [Weekday]
weekdays = cycle [Mon, Tue, Wed, Thu, Fri, Sat, Sun]

isLeap :: Int -> Bool
isLeap n = (n `mod` 4 == 0) && (n `mod` 100 /= 0 || n `mod` 400 == 0)

daysLeap :: [Int]
daysLeap = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

daysCommon :: [Int]
daysCommon = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
