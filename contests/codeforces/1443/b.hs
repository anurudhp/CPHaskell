{-# LANGUAGE ViewPatterns #-}

import Control.Arrow ((>>>))
import Data.List (dropWhileEnd, group)

main :: IO ()
main = interact $ words >>> drop 1 >>> process >>> map show >>> unlines

process :: [String] -> [Int]
process [] = []
process ((read -> a):(read -> b):xs:xss) = solve a b xs : process xss

solve :: Int -> Int -> String -> Int
solve a b = sum . map cost . group . dropWhile (== '0') . dropWhileEnd (== '0')
  where
    cost ys
      | head ys == '0' = min 0 $ b * length ys - a
      | otherwise = a
