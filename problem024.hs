solve = permutations !! 999999

items :: [Int]
items = [0..9]

permutations :: [[Int]]
permutations = do
  a <- items
  b <- filter (notelem [a]) items
  c <- filter (notelem [a, b]) items
  d <- filter (notelem [a, b, c]) items
  e <- filter (notelem [a, b, c, d]) items
  f <- filter (notelem [a, b, c, d, e]) items
  g <- filter (notelem [a, b, c, d, e, f]) items
  h <- filter (notelem [a, b, c, d, e, f, g]) items
  i <- filter (notelem [a, b, c, d, e, f, g, h]) items
  j <- filter (notelem [a, b, c, d, e, f, g, h, i]) items
  return [a, b, c, d, e, f, g, h, i, j]

notelem :: Eq a => [a] -> a -> Bool
notelem xs = not . flip elem xs
