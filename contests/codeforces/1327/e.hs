-- AC https://codeforces.com/contest/1327/submission/94919421

import Control.Arrow ((>>>))

main :: IO ()
main = interact $ read >>> solve >>> map show >>> unwords

solve :: Integer -> [Integer]
solve n = map (solveEach n) [1 .. n]

solveEach :: Integer -> Integer -> Integer
solveEach n i
  | i == n = 10
  | otherwise = 9 *% 10 ^% (n - 1 - i) *% (20 +% 9 *% (n - 1 - i))

modV :: Integer
modV = 998244353

(+%) :: Integer -> Integer -> Integer
a +% b = (a + b) `mod` modV

infixl 6 +%

(*%) :: Integer -> Integer -> Integer
a *% b = (a * b) `mod` modV

infixl 7 *%

(^%) :: Integer -> Integer -> Integer
a ^% n
  | n == 0 = 1
  | odd n = a *% a ^% (n - 1)
  | even n = let u = a ^% (n `div` 2) in u *% u

infixl 8 ^%
