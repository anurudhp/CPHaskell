-- PROBLEM https://cses.fi/problemset/task/1617/
import Control.Arrow ((>>>))

main :: IO ()
main = interact $ words >>> head >>> read >>> solve >>> show

solve :: Int -> Int
solve 0 = 1
solve n = (2 * solve (n - 1)) `mod` m
  where
    m = 10 ^ 9 + 7
