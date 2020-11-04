import Control.Arrow ((>>>))
import Data.Bool (bool)

main :: IO ()
main =
  interact $
  lines >>>
  drop 1 >>>
  map (words >>> map read) >>> process >>> map (bool "NO" "YES") >>> unlines

process :: [[Int]] -> [Bool]
process [] = []
process (_:xs:xss) = solve xs : process xss

solve :: [Int] -> Bool
solve xs = minimum pre >= 0 && minimum suf >= 0
  where
    gaps = zipWith (-) (tail xs) xs
    decs = map (min 0) gaps
    pre = scanl (+) (head xs) decs
    suf = zipWith (-) xs pre
