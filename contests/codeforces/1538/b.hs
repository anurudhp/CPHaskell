import Control.Arrow ((>>>))

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = let (hs, ts) = splitAt k xs in hs : chunksOf k ts

main :: IO ()
main = interact $ lines >>> drop 1 >>> chunksOf 2
         >>> map (last >>> words >>> map read >>> solve >>> show) >>> unlines

solve :: [Int] -> Int
solve xs
  | r == 0 = length $ filter (> q) xs
  | otherwise = -1
  where
    (q, r) = divMod (sum xs) (length xs)
