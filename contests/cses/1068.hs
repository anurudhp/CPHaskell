-- PROBLEM https://cses.fi/problemset/task/1068/
-- NAME Weird Algorithm
{-# LANGUAGE Safe #-}

import safe Control.Arrow ((>>>))

main :: IO ()
main = interact $ read >>> solve >>> map show >>> unwords

solve :: Int -> [Int]
solve n = takeWhile (/= 0) s
  where
    s = n : map f s
    f x
      | x == 1 = 0
      | odd x = 3 * x + 1
      | even x = x `div` 2
