import Control.Arrow ((>>>))

main :: IO ()
main = interact $ words >>> map read >>> solve >>> show

solve :: [Int] -> Int
solve [n, k] = sum $ zipWith (*) xs (drop (abs k) xs)
  where
    xs = [1 .. n - 1] ++ [n,n - 1 .. 1]
