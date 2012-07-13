import Data.Ratio
import Control.Monad

solve = product test

test = do
  numerator   <- [10..99]
  denominator <- [(numerator + 1)..99]
  let n' = numerator `mod` 10
      d' = denominator `div` 10
  guard $ n' == d' && (denominator `mod` 10) /= 0 
  guard $ numerator % denominator == (numerator `div` 10) % (denominator `mod` 10)
  return $ numerator % denominator
