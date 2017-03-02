import Data.List (elemIndex)

fibs = 0 : scanl (+) 1 fibs

main = print $ elemIndex 1000 $ map (length . show) fibs