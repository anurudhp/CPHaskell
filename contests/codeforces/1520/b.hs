import Control.Arrow
import Data.List

main = interact $ words >>> drop 1 >>> map (read >>> solve >>> show) >>> unlines

ordinary :: [Integer]
ordinary = [d*(10^l - 1) `div` 9 | d <- [1..9], l <- [1..9]]

solve :: Integer -> Int
solve n = length $ filter (<= n) ordinary
