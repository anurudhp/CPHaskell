{-# LANGUAGE Safe #-}

import safe Control.Arrow ((>>>))
import safe Data.Tuple (swap)

main :: IO ()
main = interact $ lines >>> drop 1 >>> map (words >>> map read) >>> process >>> map show >>> unlines

process :: [[Int]] -> [Int]
process [] = []
process (_ : xs : xss) = solve xs : process xss

solve :: [Int] -> Int
solve xs
  | all (== head xs) xs = -1
  | otherwise = snd . maximum . filter (ok . fst) . zipWith (curry swap) [1 ..] $ zip3 xs (tail xs ++ [inf]) (inf : xs)
  where
    ok (x, y, z) = x > min y z
    inf = 10 ^ 9 + 1
