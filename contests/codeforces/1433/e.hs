{-# LANGUAGE Safe #-}

import safe Control.Arrow ((>>>))

main :: IO ()
main = interact $ words >>> head >>> read >>> solve >>> show

solve :: Integer -> Integer
solve n = product [1 .. n - 1] `div` (n `div` 2)
