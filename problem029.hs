import Data.List

solve :: Int
solve = length items

items :: [Integer]
items = nub [a ^ b | a <- [2..100], b <- [2..100]]
