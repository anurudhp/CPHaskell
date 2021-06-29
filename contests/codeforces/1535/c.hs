import Control.Applicative (liftA2)
import Control.Arrow ((>>>))
import Data.List (group)

main :: IO ()
main = interact $ lines >>> drop 1 >>> map (solve >>> show) >>> unlines

solve :: String -> Integer
solve s = compute "01" s + compute "10" s - compute "?" s

compute :: String -> String -> Integer
compute st = sum . map (count  . length) . filter head . group . zipWith match (concat (repeat st))
  where
    match _ '?' = True
    match a b = a == b

count :: Int -> Integer
count n = let n' = toInteger n in (n' * (n' + 1)) `div` 2
