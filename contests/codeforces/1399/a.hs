-- AC https://codeforces.com/contest/1399/submission/94924726
{-# LANGUAGE Safe #-}

import safe Control.Arrow ((>>>))
import safe Data.List (sort)

main :: IO ()
main =
  interact $
    lines >>> drop 1
      >>> map (words >>> map read)
      >>> process
      >>> map (\b -> if b then "YES" else "NO")
      >>> unlines

process :: [[Int]] -> [Bool]
process [] = []
process ([_] : xs : rest) = solve xs : process rest

solve :: [Int] -> Bool
solve [_] = True
solve xs = let xs' = sort xs in maximum (zipWith (-) (drop 1 xs') xs') <= 1
