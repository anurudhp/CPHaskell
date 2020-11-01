import Control.Arrow ((>>>))

main :: IO ()
main = interact $ words >>> map read >>> solve >>> show

{-
-- Possible Improvements:
--   better way to do: 
--     for i from n to 1: ls[i] += ls[2 * i]
-}
-- recurrence: dp[n][k] = dp[n][2k] + dp[n-1][k-1]
solve :: [Int] -> Int
solve [n, k] = iterate upd [1] !! n !! k
  where
    upd dp = 0 : foldl1 add (alternates dp)
    add [] ys = ys
    add xs [] = xs
    add (x:xs) (y:ys) = x +% y : add xs ys

alternate :: [Int] -> [Int]
alternate [] = []
alternate [_] = []
alternate (_:x':xs) = x' : alternate xs

alternates :: [Int] -> [[Int]]
alternates = takeWhile (not . null) . iterate alternate

modv :: Int
modv = 998244353

(+%) :: Int -> Int -> Int
u +% v = (u + v) `mod` modv

infixl 6 +%
