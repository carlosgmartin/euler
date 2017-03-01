digits 0 = []
digits x = digits (div x 10) ++ [mod x 10]

main = print $ sum $ digits $ product [1..100]
