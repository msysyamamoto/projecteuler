import Control.Monad
main :: IO ()
main = print solve

solve :: Integer
solve = let (pn, pk, pj) = head pentagonal
         in abs (pk - pj)

pentagonalNumber :: Integral a => a -> a 
pentagonalNumber n = (n * (3 * n - 1)) `div` 2

pentagonalNumbers :: [Integer]
pentagonalNumbers = map pentagonalNumber [1..]

pentagonal = do
  pn <- pentagonalNumbers
  let smallPn = takeWhile (< pn) pentagonalNumbers
  pk <- smallPn
  let pj = pn - pk
  guard $ pj `elem` smallPn 
  guard $ abs (pk - pj) `elem` smallPn
  return $ (pn, pk, pj)
