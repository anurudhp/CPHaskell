-- AC https://codeforces.com/contest/1328/submission/74436739
{-# LANGUAGE Safe #-}

import safe Control.Arrow ((>>>))
import safe Data.Bifunctor (Bifunctor (bimap))
import safe Data.Bool (bool)

main :: IO ()
main =
  interact $
    lines >>> drop 1 >>> process >>> unlines

process :: [String] -> [String]
process [] = []
process (_ : s : ss) = let (u, v) = solve s in u : v : process ss

solve :: String -> (String, String)
solve "" = ("", "")
solve ('1' : cs) = ('1' : replicate (length cs) '0', '0' : cs)
solve (c : cs) = bimap (d :) (d :) (solve cs)
  where
    d = bool '0' '1' (c == '2')
