import Data.Char (isAlpha)

ones = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
tens = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

pronounce n
    | n < 20    = ones !! n
    | n < 100   = tens !! (div n 10) ++ " " ++ pronounce (mod n 10)
    | n < 1000  = ones !! (div n 100) ++ " hundred" ++ if hundreds == "" then "" else " and " ++ hundreds
    | n == 1000 = "one thousand"
    where hundreds = pronounce (mod n 100)

total = concat [pronounce n | n <- [1..1000]]

count [] = 0
count (x:xs) = (if isAlpha x then 1 else 0) + count xs

main = print $ count total
