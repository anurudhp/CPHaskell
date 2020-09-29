-- AC https://codeforces.com/contest/1328/submission/74436739

import Control.Arrow

main = interact $ 
  lines >>> drop 1 >>> process >>> unlines

process :: [String] -> [String]
process [] = []
process (n:x:rest) = fst ans : snd ans : process rest 
  where ans = solve x

solve :: String -> (String, String)
solve "" = ("", "")
solve (c:rest)
  | c == '1' = ('1' : (take (length rest) (repeat '0')),
                '0' : rest)
  | otherwise = (d : fst rec, d : snd rec)
    where
      d = if c == '2' then '1' else '0'
      rec = solve rest
