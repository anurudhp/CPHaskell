-- AC https://codeforces.com/contest/1328/submission/74403174

import Control.Arrow

main = interact $ 
  lines >>> drop 1 
  >>> map ((words >>> map read) >>> solve >>> show) 
  >>> unlines

solve :: [Integer] -> Integer
solve [a, b] 
  | d == 0 = 0
  | otherwise = b - d
    where d = a `mod` b
