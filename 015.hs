count 0 0 = 1
count r 0 = count (pred r) 0
count 0 d = count 0 (pred d)
count r d = count (pred r) d + count r (pred d)

binomial n 0 = 1
binomial 0 k = 0
binomial n k = div (binomial (pred n) (pred k) * n) k

centralBinomial n = binomial (2 * n) n

main = print $ centralBinomial 20
