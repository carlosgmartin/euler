import Data.Array (listArray, (!))
import Data.List (group)

primes = 2:filter isPrime [3,5..]
isPrime = null . tail . primeFactors
primeFactors n = factor n primes where
    factor n (p:ps)
        | p * p > n    = [n]
        | mod n p == 0 = p:factor (div n p) (p:ps)
        | otherwise    = factor n ps

sigma = product . map (succ . sum . zipWith (flip (^)) [1..]) . group . primeFactors
aliquot n = sigma n - n
abundant n = aliquot n > n

limit = 28123

abundantsArray = listArray (1, limit) $ map abundant [1..limit]
abundants = filter (abundantsArray !) [1..limit]
remainders n = map (n-) $ takeWhile (<= div n 2) abundants

main = print . sum $ filter (not . any (abundantsArray !) . remainders) [1..limit]
