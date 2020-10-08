-- AC https://codeforces.com/contest/1327/submission/94883698

import Control.Arrow ((>>>))

main :: IO ()
main =
  interact $
    lines >>> map (words >>> map read)
      >>> solve
      >>> (\ans -> [show (length ans), ans])
      >>> unlines

solve :: [[Int]] -> String
solve ([n, m, _] : _) =
  take (2 * n * m) $
    replicate (n - 1) 'U' ++ replicate (m - 1) 'L'
      ++ concat
        ( replicate
            n
            (replicate (m - 1) 'R' ++ "D" ++ replicate (m - 1) 'L' ++ "D")
        )
