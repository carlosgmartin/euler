primes = 2 : sieve [3..] 4 primes where
    sieve (n:ns) bound (p:ps)
        | n < bound = n : sieve ns bound (p:ps)
        | otherwise = sieve [m | m <- ns, mod m p /= 0] (head ps ^ 2) ps

smallestFactor x = head [y | y <- primes, mod x y == 0]

largestPrimeFactor x
    | y == x    = x
    | otherwise = largestPrimeFactor $ quot x $ y
    where
        y = smallestFactor x

main = print $ largestPrimeFactor 600851475143
