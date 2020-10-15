{-# LANGUAGE Safe #-}

import safe Control.Arrow ((>>>))

main :: IO ()
main = interact $ lines >>> drop 1 >>> map (words >>> map read) >>> solve >>> show

solve :: [[Int]] -> Int
solve [vs, cs] = sum . filter (> 0) $ zipWith (-) vs cs
