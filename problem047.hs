import Euler
import Data.List

main = print solve

solve :: Int
solve = fourConsecutiveInt [1..] 

fourConsecutiveInt :: [Int] -> Int
fourConsecutiveInt (a: bs@(b: cs@(c: ds@(d: ns))))
    | not4Elem fs4                                          = fourConsecutiveInt ns 
    | or [not4Elem fs3, fs4 == fs3, fs4 == fs2, fs4 == fs1] = fourConsecutiveInt ds 
    | or [not4Elem fs2, fs3 == fs2, fs3 == fs1]             = fourConsecutiveInt cs 
    | or [not4Elem fs1, fs2 == fs1]                         = fourConsecutiveInt bs 
    | otherwise                                             = a 
  where
    fs1 = nub $ factorization a 
    fs2 = nub $ factorization b
    fs3 = nub $ factorization c 
    fs4 = nub $ factorization d 
    not4Elem = (4 /=) . length

fourConsecutiveInt _ = error "Oops!"
