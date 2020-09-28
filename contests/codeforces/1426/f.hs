-- AC https://codeforces.com/contest/1426/submission/94155937

import Control.Arrow ((>>>))

main :: IO ()
main =
  interact $
    lines
      >>> last
      >>> solve 1 0 0 0
      >>> show
      >>> (++ "\n")

myMod :: Integer
myMod = 10 ^ 9 + 7

madd :: Integer -> Integer -> Integer
madd a b = (a + b) `mod` myMod

mmul :: Integer -> Integer -> Integer
mmul a b = (a * b) `mod` myMod

solve :: Integer -> Integer -> Integer -> Integer -> String -> Integer
solve _ _ _ abc [] = abc
solve n a ab abc (x : xs)
  | x == 'a' = solve n (madd a n) ab abc xs
  | x == 'b' = solve n a (madd ab a) abc xs
  | x == 'c' = solve n a ab (madd abc ab) xs
  | x == '?' =
    solve
      (mmul 3 n)
      (madd (mmul 3 a) n)
      (madd (mmul 3 ab) a)
      (madd (mmul 3 abc) ab)
      xs
