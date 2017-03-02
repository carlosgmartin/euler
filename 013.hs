readInt k = read k::Integer

main = readFile "013.txt" >>= print . readInt . take 10 . show . sum . map readInt . lines
