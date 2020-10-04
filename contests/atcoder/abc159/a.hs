-- AC https://atcoder.jp/contests/abc159/submissions/17192917

import Control.Arrow ((>>>))

main :: IO ()
main = interact $ words >>> map (read :: String -> Int) >>> solve >>> show

solve :: [Int] -> Int
solve xs = sum $ (\x -> (x * (x - 1)) `div` 2) <$> xs
