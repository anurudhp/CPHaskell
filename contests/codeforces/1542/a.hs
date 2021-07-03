import Control.Arrow ((>>>))
import Data.Bool (bool)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = let (hs, ts) = splitAt k xs in hs : chunksOf k ts

main :: IO ()
main = interact $
  lines
    >>> drop 1
    >>> map (words >>> map read)
    >>> chunksOf 2
    >>> map (solve >>> bool "No" "Yes")
    >>> unlines

solve :: [[Int]] -> Bool
solve [[n], xs] = length (filter odd xs) == n
