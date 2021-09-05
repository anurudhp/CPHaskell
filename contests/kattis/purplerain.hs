-- https://open.kattis.com/problems/purplerain
-- challenge from: https://byorgey.wordpress.com/2021/08/11/competitive-programming-in-haskell-monoidal-accumulation/

{-# LANGUAGE ViewPatterns #-}
import Control.Arrow ((>>>), first)

main = interact $ words >>> head >>> solve >>> showPair
  where
    showPair (a, b) = show a ++ " " ++ show b

solve :: String -> (Int, Int)
solve (map convert -> s) = snd $ min (compute s) (compute . map negate $ s)

convert 'R' = 1
convert 'B' = -1

compute :: [Int] -> (Int, (Int, Int))
compute = scanl pick (0, (1, 0)) >>> map (first negate) >>> minimum
  where
    -- previous range [l, r]
    pick (diff, (l, r)) c
      | diff + c >= 0 = (diff + c, (l, r + 1))
      | otherwise = (0, (r + 2, r + 1))

