-- AC https://codeforces.com/contest/1327/submission/74123414

import Control.Arrow

main = interact $ read >>> solve >>> map show >>> unwords

solve :: Integer -> [Integer]
solve n = map solveEach $ zip [1..n] $ repeat n

solveEach :: (Integer, Integer) -> Integer
solveEach (i, n)
  | i == n = 10
  | otherwise = 
    foldl mulv 1 
      [9, pwr 10 (n - 1 - i),
        addv 20 $ mulv 9 (n - 1 - i)]
  where
    modv = 998244353
    addv a b = mod (a + b) modv
    mulv a b = mod (a * b) modv 
    pwr :: Integer -> Integer -> Integer
    pwr a 0 = 1
    pwr a n
      | odd n = mulv a $ pwr a (n - 1)
      | even n = let u = pwr a (div n 2) in u * u