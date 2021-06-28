import Control.Arrow ((>>>))
import Data.List (sort)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs =
  let (hs, ts) = splitAt k xs
   in hs : chunksOf k ts

main :: IO ()
main =
  interact $
  lines >>>
  drop 1 >>>
  map (words >>> map read) >>> chunksOf 2 >>> map (solve >>> show) >>> unlines

type I64 = Integer

solve :: [[Int]] -> I64
solve [[n, l, r], xs] =
  let xs' = sort xs
   in (`div` 2) $
      compute xs' r - compute xs' (l - 1) -
      (toInteger . length . filter (\x -> l <= 2 * x && 2 * x <= r) $ xs)

compute :: [Int] -> Int -> I64
compute xs lim =
  sum . map (toInteger . snd . head) . tail $
  scanl
    (\vs x -> dropWhile ((> lim - x) . fst) vs)
    (reverse $ zip ((-10 ^ 9) : xs) [0 ..])
    xs
