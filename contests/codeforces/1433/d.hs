{-# LANGUAGE Safe #-}

import safe Control.Arrow ((>>>))
import safe Data.Bool (bool)
import safe Data.List (intercalate)

main :: IO ()
main = interact $ lines >>> drop 1 >>> map (words >>> map read) >>> process >>> unlines

process :: [[Int]] -> [String]
process [] = []
process (_ : xs : xss) = solve xs : process xss

solve :: [Int] -> String
solve xs
  | all (== a) xs = "NO"
  | otherwise = intercalate "\n" ("YES" : map (unwords . map show) edges)
  where
    a = head xs
    ys = zip xs [1 ..]
    i = snd . head $ filter ((/= a) . fst) ys
    edges = map (\(b, j) -> [bool 1 i (a == b), j]) (tail ys)
