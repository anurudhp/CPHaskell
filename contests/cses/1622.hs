-- PROBLEM https://cses.fi/problemset/task/1622
import Control.Arrow ((>>>))
import Data.List (delete, sort)

main :: IO ()
main = interact $ words >>> head >>> solve >>> prepLength >>> unlines
  where
    prepLength xs = show (length xs) : xs

solve :: String -> [String]
solve = sort . permute . sort

permute :: String -> [String]
permute [x] = [[x]]
permute xs =
  concat [(c :) <$> permute (delete c xs) | c <- ['a' .. 'z'], c `elem` xs]
