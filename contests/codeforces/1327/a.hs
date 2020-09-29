-- AC https://codeforces.com/contest/1327/submission/74075716

import Control.Arrow
import Data.Array as Array

main = interact $ 
  lines >>> drop 1 >>> map (words >>> map read >>> solve) >>> unlines >>> (++"\n")
 
solve :: [Integer] -> String
solve [n, k]
  | n < k * k = "NO"
  | odd (n - k * k) = "NO"
  | otherwise = "YES"
