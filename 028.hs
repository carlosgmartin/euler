sumCorners 0 = 1
sumCorners n = (2*n + 1)^2 + (2*n + 1)^2 - 2*n + (2*n + 1)^2 - 4*n + (2*n + 1)^2 - 6*n

sumDiagonals n = sum $ [sumCorners k | k <- [0..quot (pred n) 2]]

main = print $ sumDiagonals 1001
