import Control.Arrow ((>>>))

main :: IO ()
main = interact $ words >>> drop 1 >>> map (read >>> solve >>> show) >>> unlines

solve :: Int -> Int
solve n
  | n <= 3 = n - 1
  | even n = 2
  | odd n = 3
