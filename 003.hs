smallestFactor x = head [y | y <- [2..x], mod x y == 0]

largestPrimeFactor x = if smallestFactor x == x then x else largestPrimeFactor $ quot x $ smallestFactor x

main = print $ largestPrimeFactor 600851475143
