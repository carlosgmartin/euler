import Data.List (sort)
import Data.Char (ord)

split delims str = split' delims str [] where
    split' delims str accum
        | null str                  = x
        | elem (head str) delims    = x ++ split' delims (tail str) []
        | otherwise                 = split' delims (tail str) (accum ++ [head str])
        where x = if null accum then [] else [accum]

score = sum . map (\c -> ord c - ord 'A' + 1)

main = readFile "names.txt" >>= print . sum . zipWith (*) [1..] . map score . sort . split [',', '"']
