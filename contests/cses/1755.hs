-- PROBLEM https://cses.fi/problemset/task/1755
import Control.Arrow ((>>>))
import Data.Maybe (fromMaybe)

main :: IO ()
main = interact $ solve >>> fromMaybe "NO SOLUTION"

solve :: String -> Maybe String
solve s
  | length mid > 1 = Nothing
  | otherwise = Just $ xs ++ mid ++ reverse xs
  where
    fs = [(c, length (filter (== c) s)) | c <- ['A' .. 'Z']]
    mid = map fst . filter (odd . snd) $ fs
    xs = concat [replicate (l `div` 2) c | (c, l) <- fs]
