import Control.Applicative (liftA2)
import Control.Arrow ((>>>))

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = let (hs, ts) = splitAt k xs in hs : chunksOf k ts

main :: IO ()
main = interact $ lines >>> drop 1 >>> chunksOf 2
         >>> map (last >>> words >>> map read >>> solve >>> show) >>> unlines

solve :: [Int] -> Int
solve xs = n - maximum [l, r, m]
  where
    n = length xs
    check = liftA2 (&&) (/= 1) (/= n)
    l = length $ takeWhile check xs
    r = length $ takeWhile check $ reverse xs
    m = n - l - r - 2
