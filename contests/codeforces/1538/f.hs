import Control.Arrow ((>>>))

main :: IO ()
main = interact $ lines >>> drop 1 >>>
         map (words >>> map read >>> solve >>> show) >>> unlines

solve :: [Int] -> Int
solve [l, r] = cost r - cost l
  where
    cost = sum . takeWhile (> 0) . iterate (`div` 10)
