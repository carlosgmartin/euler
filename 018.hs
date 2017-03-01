parse :: String -> [[Integer]]
parse = map (map read) . map words . lines

addCells top bottomLeft bottomRight = top + max bottomLeft bottomRight

addRows top bottom = zipWith3 addCells top bottom $ tail bottom

main = do
    content <- readFile "018.txt"
    print . head . foldr1 addRows $ parse content
