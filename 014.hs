import Data.Array (listArray, assocs, (!))
import Data.List (maximumBy)
import Data.Ord (comparing)
 
makeTable size = table where 
    table = listArray (1, size) $ map chainLength [1..size]
    chainLength x
        | x == 1    = 0
        | y <= size = succ $ table ! y
        | otherwise = succ $ chainLength y
        where 
        y = if even x then div x 2 else 3 * x + 1

main = print . fst . maximumBy (comparing snd) . assocs . makeTable $ 1000000
