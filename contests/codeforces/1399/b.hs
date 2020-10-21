-- AC https://codeforces.com/contest/1399/submission/94944294
{-# LANGUAGE Safe #-}

import safe Control.Arrow ((>>>))

main :: IO ()
main =
  interact $
    lines >>> drop 1
      >>> map (words >>> map read)
      >>> process
      >>> map show
      >>> unlines

process :: [[Integer]] -> [Integer]
process [] = []
process ([_] : xs : xs' : xss) = solve xs xs' : process xss

solve :: [Integer] -> [Integer] -> Integer
solve xs ys = sum $ zipWith max (reduce xs) (reduce ys)
  where
    reduce zs = subtract (minimum zs) <$> zs
