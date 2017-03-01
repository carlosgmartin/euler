
primes = 2 : sieve [3..] 4 primes where
    sieve (n:ns) bound (p:ps)
        | n < bound = n : sieve ns bound (p:ps)
        | otherwise = sieve [m | m <- ns, mod m p /= 0] (head ps^2) ps

factor n p a
    | n == 1                    = [a]
    | mod n (primes !! p) == 0  = factor (div n (primes !! p)) p (succ a)
    | otherwise                 = [a] ++ (factor n (succ p) 0)

factor' n = factor n 0 0

countDivisors n = product $ map succ $ factor' n

triangles = 1 : zipWith (+) triangles [2..]

main = print $ head [t | t <- triangles, countDivisors t > 500]
