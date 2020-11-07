import Control.Arrow ((>>>))

main :: IO ()
main =
  interact $ lines >>> drop 1 >>> map (words >>> map read) >>> solve >>> show

solve :: [[Int]] -> Int
solve = sum . map f
  where
    f [l, r] = sumN r - sumN (l - 1)
    sumN n = n * (n + 1) `div` 2
