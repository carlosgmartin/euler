import Data.List (group)

primes = 2:filter isPrime [3,5..]
isPrime = null . tail . primeFactors
primeFactors n = factor n primes where
    factor n (p:ps)
        | p * p > n    = [n]
        | mod n p == 0 = p:factor (div n p) (p:ps)
        | otherwise    = factor n ps

countDivisors = product . map (succ . length) . group . primeFactors

triangulars = scanl1 (+) [1..]

main = print $ head [t | t <- triangulars, countDivisors t > 500]
