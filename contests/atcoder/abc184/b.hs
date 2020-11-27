import Control.Arrow ((>>>))

main = interact $ lines >>> solve >>> show

solve :: [String] -> Int
solve [nx, s] = foldl f x s
  where
    [_, x] = words >>> map read $ nx
    f v 'o' = v + 1
    f 0 'x' = 0
    f v 'x' = v - 1
