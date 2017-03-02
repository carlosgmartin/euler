leap year
    | mod year 400 == 0     = True
    | mod year 100 == 0     = False
    | mod year 4 == 0       = True
    | otherwise             = False

days (month, year)
    | month == 1                = if leap year then 29 else 28
    | elem month [3, 5, 8, 10]  = 30
    | otherwise                 = 31

next (month, year)
    | month == 11   = (0, succ year)
    | otherwise     = (succ month, year)

count start end weekday
    | start == end  = current
    | otherwise     = current + count (next start) end (mod (weekday + days start) 7)
    where current = if weekday == 0 then 1 else 0

weekday_1900 = 1
weekday_1901 = mod (weekday_1900 + sum [days (month, 1990) | month <- [0..11]]) 7

main = print $ count (0, 1901) (11, 2000) weekday_1901
