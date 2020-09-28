-- AC https://codeforces.com/contest/1426/submission/94139558

import Control.Arrow ((>>>))

main :: IO ()
main =
  interact $
    lines >>> drop 1
      >>> map (read >>> solve >>> show)
      >>> unlines

solve :: Integer -> Integer
solve 1 = 0
solve n =
  let r = round $ sqrt $ fromIntegral n
   in case compare (r * r) n of
        EQ -> 2 * r - 2
        LT -> 2 * r - 1
        GT -> 2 * r - 2
