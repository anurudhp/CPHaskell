-- PROBLEM https://codeforces.com/contest/1/problem/A

import Control.Arrow ((>>>))

main :: IO ()
main =
  interact $
    words >>> map read >>> solve >>> show >>> (++ "\n")

solve :: [Integer] -> Integer
solve [n, m, a] = ceil n a * ceil m a
  where
    ceil x y = (x + y - 1) `div` y
