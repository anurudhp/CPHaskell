-- PROBLEM https://cses.fi/problemset/task/1069
import Control.Arrow ((>>>))
import Data.List (group)

main :: IO ()
main = interact $ solve >>> show

solve :: String -> Int
solve = maximum . map length . group
