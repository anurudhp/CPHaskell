{-# LANGUAGE Safe #-}

import safe Control.Arrow ((>>>))

main :: IO ()
main = interact $ words >>> drop 1 >>> map read >>> solve >>> show

solve :: [Integer] -> Integer
solve xs
  | even negs || zeros > 0 = sum axs
  | otherwise = sum axs - 2 * minimum axs
  where
    count c = length . filter c
    zeros = count (== 0) xs
    negs = count (< 0) xs
    axs = abs <$> xs
