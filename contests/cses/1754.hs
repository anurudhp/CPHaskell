-- PROBLEM https://cses.fi/problemset/task/1754/
import Control.Arrow ((>>>))
import Data.Bool (bool)

main :: IO ()
main =
  interact $
  lines >>>
  drop 1 >>> map (words >>> map read >>> solve >>> bool "NO" "YES") >>> unlines

solve :: [Int] -> Bool
solve [a, b] = 2 * a >= b && 2 * b >= a && (a + b) `mod` 3 == 0
