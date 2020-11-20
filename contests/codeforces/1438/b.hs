import Control.Arrow ((>>>))
import Data.Bool (bool)
import Data.List (group, sort)

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
solve = any ((> 1) . length) . group . sort
