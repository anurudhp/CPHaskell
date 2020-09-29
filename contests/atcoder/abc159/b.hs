-- AC https://atcoder.jp/contests/abc159/submissions/11106529

import Control.Arrow

main =
  interact $
    lines >>> head >>> solve

solve :: String -> String
solve s
  | let len = length s
     in and
          [ isPalin s,
            isPalin $ take (div len 2) s,
            isPalin $ drop (1 + (div len 2)) s
          ] =
    "Yes"
  | otherwise = "No"

isPalin :: String -> Bool
isPalin s = s == reverse s
