import Data.List (transpose)

parse :: String -> [[Integer]]
parse = map (map read) . map words . lines

slide len list
    | len <= length list    = take len list : slide len (tail list)
    | otherwise             = []

grid = [[0..3],[4..7],[8..11],[12..15]]
hor = id
ver = transpose
udiag = transpose . zipWith drop [0..]
ldiag = transpose . map reverse . zipWith take [0..]
udiagr = udiag . reverse
ldiagr = ldiag . reverse
traces grid = concat $ map ($ grid) [hor, ver, udiag, ldiag, udiagr, ldiagr]

main = do
    content <- readFile "011.txt"
    print . maximum . map product . concat . map (slide 4) . traces $ parse content
