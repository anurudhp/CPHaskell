-- AC https://atcoder.jp/contests/abc159/submissions/11095079

import Control.Arrow

main = interact $ 
  words >>> map (read:: String->Int) >>> solve >>> show

solve :: [Int] -> Int
solve xs = foldl (+) 0 $ map (\x -> div (x * (x - 1)) 2) xs
