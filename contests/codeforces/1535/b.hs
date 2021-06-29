import Control.Applicative (liftA2)
import Control.Arrow ((>>>))

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = let (hs, ts) = splitAt k xs in hs : chunksOf k ts

main :: IO ()
main = interact $ lines >>> drop 1 >>> chunksOf 2
         >>> map (last >>> words >>> map read >>> solve >>> show) >>> unlines

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

solve :: [Int] -> Int
solve xs = (`div` 2) $ count id [True | x <- xs, y <- xs, even x || even y || gcd x y > 1] - count (> 1) xs
