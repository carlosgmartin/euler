import Data.Set (fromList)

main = print $ length $ fromList [a^b | a <- [2..100], b <- [2..100]]
