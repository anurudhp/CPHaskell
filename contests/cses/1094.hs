{-# LANGUAGE ParallelListComp #-}

-- PROBLEM https://cses.fi/problemset/task/1094
import Control.Arrow ((>>>))

main :: IO ()
main = interact $ words >>> drop 1 >>> map read >>> solve >>> show

solve :: [Int] -> Int
solve xs = sum [mx - x | mx <- scanl1 max xs | x <- xs]
