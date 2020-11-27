import Control.Arrow ((>>>))
import Data.List (group, sort)

main =
  interact $
  lines >>>
  drop 1 >>> map (words >>> map read) >>> process >>> map show >>> unlines

process :: [[Int]] -> [Int]
process [] = []
process (_:xs:xss) = solve xs : process xss

solve :: [Int] -> Int
solve xs
  | length xs' == 1 = 0
  | length xs' == 2 = 1
  | head xs' `notElem` vs = 1
  | last xs' `notElem` vs = 1
  | otherwise = (1 +) . minimum . map length . group . sort $ vs
  where
    xs' = map head . group $ xs
    vs = init . tail $ xs'
