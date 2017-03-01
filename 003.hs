smallestFactor x = head [y | y <- [2..x], mod x y == 0]

largestPrimeFactor x
    | y == x    = x
    | otherwise = largestPrimeFactor $ quot x $ y
    where
        y = smallestFactor x

main = print $ largestPrimeFactor 600851475143
