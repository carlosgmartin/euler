primes = 2:filter isPrime [3,5..]
isPrime n
    | n <= 1    = False
    | otherwise = (null . tail . primeFactors) n
primeFactors n = factor n primes where
    factor n (p:ps)
        | p * p > n    = [n]
        | mod n p == 0 = p:factor (div n p) (p:ps)
        | otherwise    = factor n ps

values (a, b) = [n^2 + a * n + b | n <- [0..]]

count = length . takeWhile isPrime . values

main = print . snd . maximum $ [(count (a, b), a * b) | a <- [-999..999], b <- [-1000..1000]]
