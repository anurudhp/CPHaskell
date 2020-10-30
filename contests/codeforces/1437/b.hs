import Control.Arrow
import Data.List

main :: IO ()
main = interact $ lines >>> drop 1 >>> process >>> map show >>> unlines

process :: [String] -> [Int]
process [] = []
process (_:s:ss) = solve s : process ss

solve :: String -> Int
solve s =
  foldl max 0 . map length . group . sort . filter (uncurry (==)) $
  zip s (tail s)
