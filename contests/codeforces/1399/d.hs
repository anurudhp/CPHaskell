-- AC https://codeforces.com/contest/1399/submission/94928816
{-# LANGUAGE Safe #-}

import safe Control.Arrow ((>>>))

main :: IO ()
main =
  interact $
    lines >>> drop 1
      >>> process
      >>> map (map show >>> unwords)
      >>> unlines

process :: [String] -> [[Int]]
process [] = []
process (_ : xs : rest) = let (k, ss) = solve xs in [k] : ss : process rest

solve :: String -> (Int, [Int])
solve = (\(_, _, ps, k) -> (k, ps)) . foldr pick ([], [], [], 0)
  where
    pick :: Char -> ([Int], [Int], [Int], Int) -> ([Int], [Int], [Int], Int)
    pick '0' (xs, [], ps, k) = ((k + 1) : xs, [], (k + 1) : ps, k + 1)
    pick '0' (xs, y : ys, ps, k) = (y : xs, ys, y : ps, k)
    pick '1' ([], ys, ps, k) = ([], (k + 1) : ys, (k + 1) : ps, k + 1)
    pick '1' (x : xs, ys, ps, k) = (xs, x : ys, x : ps, k)
