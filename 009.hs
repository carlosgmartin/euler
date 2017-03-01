main = print $ first [a * b * c | a <- [1..1000], c <- [1..1000 - a], let b = 1000 - a - c, a^2 + b^2 == c^2, b > a]
