-- AC https://codeforces.com/contest/1328/submission/74429105

import Control.Arrow

main = interact $ 
  lines >>> drop 1 
  >>> map ((words >>> map read) >>> solve) 
  >>> unlines

solve :: [Integer] -> String
solve [n, k] = reverse $ foldl1 (++) $ map copy 
  [(j - 1, 'a'), (1, 'b'), (i - j - 1, 'a'), (1, 'b'), (n - i, 'a')] 
  where
    copy (n, c) 
      | n <= 0 = ""
      | otherwise = take (fromIntegral n) $ repeat c
    c2 v = (v * (v - 1)) `div` 2
    getfirst len rank
      | c2 len < rank = max len $ getfirst (len + 1) rank
      | otherwise = 0
    i = (getfirst 0 k) + 1
    j = k - (c2 (i - 1))
