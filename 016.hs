digits 0 = []
digits x = digits (div x 10) ++ [mod x 10]

main = print . sum $ digits (2^1000)
