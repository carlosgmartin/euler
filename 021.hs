divisors n = [k | k <- [1..div n 2], mod n k == 0]

d = sum . divisors

amicable n = let x = d n in n /= x && d x == n

main = print $ sum $ filter amicable [1..10000]
