{-# LANGUAGE Safe #-}

import safe Control.Arrow ((>>>))

main :: IO ()
main = interact $ words >>> map read >>> solve >>> show

solve :: [Int] -> Int
solve [a, b, t] = b * (t `div` a)
