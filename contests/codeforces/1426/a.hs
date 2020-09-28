-- AC https://codeforces.com/contest/1426/submission/94134818

import Control.Arrow ((>>>))

main :: IO ()
main =
  interact $
    lines >>> drop 1
      >>> map (words >>> map read >>> solve >>> show)
      >>> unlines

solve :: [Int] -> Int
solve [n, x]
  | n <= 2 = 1
  | otherwise = 2 + div (n - 3) x
