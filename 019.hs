leap year
    | mod year 400 == 0     = True
    | mod year 100 == 0     = False
    | mod year 4 == 0       = True
    | otherwise             = False

days month year
    | month == 2                = if leap year then 29 else 28
    | elem month [3, 5, 8, 10]  = 30
    | otherwise                 = 31

previous (day, month, year)
    | day == 0 && month == 0    = (days 11 (pred year), 11, pred year)
    | day == 0                  = (days (pred month) year, pred month, year)
    | otherwise                 = (pred day, month, year)

weekday (day, month, year)
    | (day, month, year) == (0, 0, 1900)    = 0
    | otherwise                             = mod (succ . weekday $ previous (day, month, year)) 7

countSundays (day, month, year) (day2, month2, year2)
    | (day, month, year) == (day2, month2, year2) = 0
    | otherwise                                   = countSundays (day, month, year) (previous (day2, month2, year2)) + if weekday (day2, month2, year2) == 6 && day2 == 0 then 1 else 0

daysBetween (d1, m1, y1) (d2, m2, y2)
    | (d1, m1, y1) == (d2, m2, y2)      = 0
    | otherwise                         = succ $ daysBetween (d1, m1, y1) $ previous (d2, m2, y2)

main = print $ daysBetween (0, 0, 1901) (30, 11, 2000)
