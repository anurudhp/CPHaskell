{-# LANGUAGE ParallelListComp #-}

import Control.Applicative (Applicative(liftA2))
import Control.Arrow ((>>>))
import Data.Graph
import Data.List (transpose)

main :: IO ()
main = interact $ lines >>> map (words >>> map read) >>> solve >>> show

solve :: [[Int]] -> Int
solve ([n, k]:xs) = compute n k xs *% compute n k (transpose xs)

compute :: Int -> Int -> [[Int]] -> Int
compute n k xs = foldl (*%) 1 $ map (factorial . length) comps
  where
    oks = all (<= k) <$> [[p + q | p <- ps | q <- qs] | ps <- xs, qs <- xs]
    ixs = liftA2 (,) [1 .. n] [1 .. n]
    edges = filter (uncurry (/=)) . map snd . filter fst $ zip oks ixs
    comps = components $ buildG (1, n) edges

factorial :: Int -> Int
factorial 0 = 1
factorial n = n *% factorial (n - 1)

mMod :: Int
mMod = 998244353

(+%) :: Int -> Int -> Int
u +% v = (u + v) `mod` mMod

infixl 6 +%

(*%) :: Int -> Int -> Int
u *% v = (u * v) `mod` mMod

infixl 7 *%
