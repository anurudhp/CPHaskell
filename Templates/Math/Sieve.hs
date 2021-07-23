module Sieve where

import Data.Array.Unboxed(UArray, accumArray)

sieve :: Int -> UArray Int Int
sieve n = accumArray min n (1, n) $ (1, 1) : do
  i <- [2..n]
  zip [i, 2*i .. n] $ repeat i
