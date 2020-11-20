-- https://cses.fi/problemset/task/1618/
import Control.Arrow ((>>>))

main :: IO ()
main = interact $ words >>> head >>> read >>> solve >>> show

solve :: Int -> Int
solve 0 = 0
solve n =
  let n' = n `div` 5
   in n' + solve n'
