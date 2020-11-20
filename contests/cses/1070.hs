-- https://cses.fi/problemset/task/1070/
import Control.Arrow ((>>>))
import Data.Maybe (fromMaybe)

main :: IO ()
main =
  interact $
  words >>>
  head >>>
  read >>> solve >>> fmap (unwords . map show) >>> fromMaybe "NO SOLUTION"

solve :: Int -> Maybe [Int]
solve n
  | n == 1 = Just [1]
  | n <= 3 = Nothing
  | otherwise =
    Just $ filter odd [5 .. n] ++ [3, 1, 4, 2] ++ filter even [5 .. n]
