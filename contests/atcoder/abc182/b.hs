import Control.Arrow ((>>>))
import Data.List (maximumBy)
import Data.Ord (comparing)

main :: IO ()
main = interact $ words >>> drop 1 >>> map read >>> solve >>> show

solve :: [Int] -> Int
solve xs = maximumBy (comparing gcdness) [2 .. 1000]
  where
    gcdness i = length $ filter ((== 0) . (`mod` i)) xs
