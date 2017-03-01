sieve (n:ns) bound (p:ps)
    | n < bound = n : sieve ns bound (p:ps)
    | otherwise = sieve [m | m <- ns, mod m p /= 0] (head ps^2) ps

primes = 2 : sieve [3..] 4 primes

main = print $ primes !! 10000