skip a n = take n a ++ drop (succ n) a

permutations a
    | length a > 1  = concat $ map (\n -> map (a !! n :) (permutations (skip a n))) [0..pred $ length a]
    | otherwise     = [a]

main = print $ (\n -> read n::Integer) $ concat $ map show $ permutations [0..9] !! pred 1000000
