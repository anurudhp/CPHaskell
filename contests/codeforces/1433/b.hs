{-# LANGUAGE Safe #-}

import safe Control.Arrow ((>>>))
import safe Data.List (dropWhileEnd)

main :: IO ()
main = interact $ lines >>> drop 1 >>> process >>> map show >>> unlines

process :: [String] -> [Int]
process [] = []
process (_ : x : xs) = solve x : process xs

solve :: [Char] -> Int
solve = length . filter (== '0') . dropWhileEnd (/= '1') . dropWhile (/= '1')
