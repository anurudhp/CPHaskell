import Control.Arrow ((>>>))
import Data.List (groupBy, sort)

main =
  interact $
  lines >>>
  drop 1 >>> map (words >>> map read) >>> process >>> map show >>> unlines

process :: [[Int]] -> [Int]
process [] = []
process (_:xs:xss) = solve xs : process xss

solve :: [Int] -> Int
solve xs
  | null ys = -1
  | otherwise = snd . head . head $ ys
  where
    ys = filter ((== 1) . length) . groupBy (eq fst) . sort $ zip xs [1 ..]
    eq p x y = p x == p y
