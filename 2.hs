fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main = print $ sum $ filter even $ takeWhile (<= floor 4e6) fibs
