{-# LANGUAGE Safe #-}

import safe Control.Arrow ((>>>))

main :: IO ()
main = interact $ words >>> drop 1 >>> map read >>> solve >>> show

solve :: [Int] -> Int
solve xs = max (compute (xs !! 0)) (compute (xs !! 1))
  where
    compute x = maximum . filter check $ factors x
    check y = (<= 1) . length . filter ((/= 0) . (`mod` y)) $ xs

factors :: Int -> [Int]
factors x = concatMap (\y -> [y, x `div` y]) . filter ((== 0) . (x `mod`)) $ [1 .. 40000]
