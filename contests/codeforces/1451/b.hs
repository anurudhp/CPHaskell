import Control.Arrow ((>>>))
import Data.Bool (bool)

main :: IO ()
main =
  interact $
  lines >>> drop 1 >>> process >>> concat >>> map (bool "NO" "YES") >>> unlines

process :: [String] -> [[Bool]]
process [] = []
process (nq:s:xss) = map (solve s) qs : process xss'
  where
    q = words >>> last >>> read $ nq
    (xs, xss') = splitAt q xss
    qs = map (words >>> map read) xs

solve :: String -> [Int] -> Bool
solve s [l, r] =
  elem (s !! (l - 1)) (take (l - 1) s) || elem (s !! (r - 1)) (drop r s)
