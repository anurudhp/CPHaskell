{-# LANGUAGE Safe #-}

import safe Control.Arrow ((>>>))

main :: IO ()
main = interact $ words >>> drop 1 >>> map read >>> solve >>> show

solve :: [Int] -> Int
solve xs = max (compute (xs !! 0)) (compute (xs !! 1))
  where
    compute = maximum . filter check . factors
    check y = count ((/= 0) . (`mod` y)) xs <= 1

factors :: Int -> [Int]
factors x =
  concatMap (\y -> [y, x `div` y]) . filter ((== 0) . (x `mod`)) $ [1 .. 40000]

count :: (Eq a) => (a -> Bool) -> [a] -> Int
count cond xs = length $ filter cond xs
