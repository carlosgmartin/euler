quotients a b = div a b : quotients (10 * mod a b) b
dividends a b = a : dividends (10 * mod a b) b

-- tortoise and hare algorithm
findCycle seq = findRepetition seq (tail seq)
  where findRepetition (t:ts) (h:hs)
         | t == h              = findStart seq ts
         | otherwise           = findRepetition ts (tail hs)
        findStart (t:ts) (h:hs)
         | t == h              = ([], t:findLength t ts)
         | otherwise           = let (prefix, repetition) = findStart ts hs in (t:prefix, repetition)
        findLength t (h:hs)
         | t == h              = []
         | otherwise           = h:findLength t hs

period a b = length . snd . findCycle $ dividends a b

main = print . snd $ maximum [(period 1 n, n) | n <- [1..999]]
