import Control.Arrow ((>>>))
import Data.List

main :: IO ()
main =
  interact $ 
    lines
      >>> drop 1 
      >>> process
      >>> unlines

process :: [String] -> [String]
process [] = []
process (_:xs:xss) = res : process xss
  where
    xs' = map read $ words xs
    ans = solve xs'
    res = if ans then "YES" else "NO"

solve :: [Int] -> Bool
solve xs = maximum gaps <= 1
  where
    xs' = sort xs
    -- gaps = map (\(x, y) -> x - y) $ zip (tail xs') xs' 
    -- gaps = zipWith (\x y -> x - y) (tail xs') xs' 
    gaps = zipWith (-) (tail xs') xs'
