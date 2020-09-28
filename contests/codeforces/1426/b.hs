-- AC https://codeforces.com/contest/1426/submission/94136150

import Control.Arrow ((>>>))

main :: IO ()
main =
  interact $
    lines >>> drop 1
      >>> map (words >>> map read)
      >>> process
      >>> map (\x -> if x then "YES" else "NO")
      >>> unlines

process :: [[Int]] -> [Bool]
process [] = []
process ([n, m] : rest) =
  (if m `mod` 2 == 0 then solve (take (n * 2) rest) else False) : process (drop (n * 2) rest)

solve :: [[Int]] -> Bool
solve [] = False
solve ([_, a] : [b, _] : rest) = or [a == b, solve rest]
