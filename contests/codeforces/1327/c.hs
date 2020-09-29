-- AC https://codeforces.com/contest/1327/submission/74113049

import Control.Arrow

main = interact $ 
  lines >>> map (words >>> map read) >>> 
  solve >>> (\ans -> [show $ length ans, ans]) >>> unlines
 
solve :: [[Int]] -> String
solve ([n,m,k]:_) = take (2 * n * m) $ 
  (take (n - 1) $ repeat 'U') ++ 
  (take (m - 1) $ repeat 'L') ++
  (foldr (++) "" $ take n $ repeat
    (take (m - 1) (repeat 'R') ++ "D" ++
    take (m - 1) (repeat 'L') ++ "D"))
