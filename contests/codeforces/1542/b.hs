import Control.Arrow ((>>>))
import Data.Bool (bool)

main :: IO ()
main = interact $
  lines
    >>> drop 1
    >>> map (words >>> map read >>> solve >>> bool "No" "Yes")
    >>> unlines

solve :: [Integer] -> Bool
solve [n, 1, b] = b == 1 || n `mod` b == 1
solve [n, a, b] = any ((== 0) . (`mod` b) . (n -)) $ takeWhile (<= n) $ iterate (a *) 1
