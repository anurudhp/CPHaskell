import Control.Arrow ((>>>))

main = interact $ words >>> map read >>> solve >>> show

solve :: [Int] -> Int
solve [a, b, c, d]
  | a == c && b == d = 0
  | a + b == c + d = 1
  | a - b == c - d = 1
  | abs (a - c) + abs (b - d) <= 3 = 1
  | even (a + b + c + d) = 2
  | abs ((a + b) - (c + d)) <= 3 = 2
  | abs ((a - b) - (c - d)) <= 3 = 2
  | otherwise = 3
