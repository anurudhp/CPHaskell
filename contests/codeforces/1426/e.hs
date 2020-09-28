-- AC https://codeforces.com/contest/1426/submission/94143124

import Control.Arrow ((>>>))

main :: IO ()
main =
  interact $
    lines
      >>> map (words >>> map read)
      >>> solve
      >>> map show
      >>> unwords

solve :: [[Integer]] -> [Integer]
solve [[n], a, b] = [worst n a b, best a b]

best :: [Integer] -> [Integer] -> Integer
best [a1, a2, a3] [b1, b2, b3] = min a1 b2 + min a2 b3 + min a3 b1

worst :: Integer -> [Integer] -> [Integer] -> Integer
worst n [a1, a2, a3] [b1, b2, b3] = max 0 (a1 + b2 - n) + max 0 (a2 + b3 - n) + max 0 (a3 + b1 - n)
