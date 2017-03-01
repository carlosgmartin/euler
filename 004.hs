main = print $ maximum [z | x <- [100..999], y <- [x..999], let z = x * y, show z == reverse $ show z]
