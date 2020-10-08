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
process ([_] : xs : ys : rest) = solve xs ys : process rest

solve :: [Integer] -> [Integer] -> Integer
solve xs ys = sum $ zipWith max (reduce xs) (reduce ys)
  where
    reduce xs = subtract (minimum xs) <$> xs
