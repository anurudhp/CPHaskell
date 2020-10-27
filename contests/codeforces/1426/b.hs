-- AC https://codeforces.com/contest/1426/submission/94136150

import Control.Arrow ((>>>))
import Data.Bool

main :: IO ()
main =
  interact $
    lines >>> drop 1
      >>> map (words >>> map read)
      >>> process
      >>> map (bool "NO" "YES")
      >>> unlines

process :: [[Int]] -> [Bool]
process [] = []
process ([n, m] : xss) =
  (even m && solve xs) : process xss'
  where
    (xs, xss') = splitAt (n * 2) xss

solve :: [[Int]] -> Bool
solve [] = False
solve ([_, x] : [x', _] : xs) = x == x' || solve xs
