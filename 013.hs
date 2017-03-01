readInt k = read k::Integer

main = do
    contents <- readFile "013.txt"
    print . readInt . take 10 . show . sum . map readInt $ lines contents
