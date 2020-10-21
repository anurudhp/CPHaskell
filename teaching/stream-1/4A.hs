-- PROBLEM https://codeforces.com/contest/4/problem/A

import Control.Arrow ((>>>))

main :: IO ()
main =
  interact $
    lines >>> head >>> read >>> solve >>> (++ "\n")

solve :: Int -> String
solve w
  | w == 2 = "NO"
  | odd w = "NO"
  | otherwise = "YES"
