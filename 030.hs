digits 0 = []
digits x = digits (div x 10) ++ [mod x 10]

maxDigits power = last $ takeWhile (\digits -> 9^power * digits > 10^(pred digits)) [1..]

sumPowers power n = sum . map (^power) $ digits n

power = 5
main = print $ sum $ filter (\n -> n == sumPowers power n) $ [10..10^(maxDigits power)]
