{-# LANGUAGE ParallelListComp #-}

import Control.Arrow ((>>>))
import Data.List (delete)

main :: IO ()
main = interact $ lines >>> map (words >>> map read) >>> solve >>> show

solve :: [[Int]] -> Int
solve ([n, k]:ts) = length . filter (== k) . map time $ perms
  where
    perms = map (\p -> 0 : p ++ [0]) $ permute [1 .. n - 1]
    time p = sum [ts !! i !! j | i <- p | j <- tail p]

permute :: [Int] -> [[Int]]
permute [] = [[]]
permute [x] = [[x]]
permute xs = concatMap (\x -> (x :) <$> permute (delete x xs)) xs
