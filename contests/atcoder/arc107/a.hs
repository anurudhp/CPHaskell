import Control.Arrow ((>>>))

main :: IO ()
main = interact $ words >>> map read >>> solve >>> show

solve :: [Int] -> Int
solve = foldl1 mul . map c2
  where
    m = 998244353
    mul x y = (x * y) `mod` m
    c2 x = mul 1 $ x * (x + 1) `div` 2
