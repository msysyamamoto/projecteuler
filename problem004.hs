solve = maximum [x * y | x <- [100..999], y <- [100..999], let str = show (x * y) :: String, str == reverse str]
