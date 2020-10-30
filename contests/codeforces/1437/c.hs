import Control.Arrow ((>>>))
import Data.List (sort)

main :: IO ()
main =
  interact $
  lines >>>
  drop 1 >>> map (words >>> map read) >>> process >>> map show >>> unlines

process :: [[Int]] -> [Int]
process [] = []
process (_:xs:xss) = solve xs : process xss

solve :: [Int] -> Int
solve = minimum . foldl upd (replicate 201 0) . sort
  where
    upd dp x = scanl1 min . zipWith (+) (inf : dp) . map (abs . (x -)) $ [0 ..]
    inf = 10 ^ 9
