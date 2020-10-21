-- PROBLEM https://codeforces.com/contest/71/problem/A

import Control.Arrow ((>>>))

main :: IO ()
main =
  interact $
    lines >>> drop 1 >>> process >>> unlines

process :: [String] -> [String]
process [] = []
process (x:xs) = solve x : process xs

solve :: String -> String
solve s
  | length s <= 10 = s
  | otherwise = head s : show len ++ [last s]
  where
    len = length s - 2
