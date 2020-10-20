{-# LANGUAGE Safe #-}

import safe Control.Arrow ((>>>))
import safe Data.List (sort)

main :: IO ()
main = interact $ words >>> drop 1 >>> map (solve >>> show) >>> unlines

solve :: String -> Int
solve x = (+ length x) . sum . map length . takeWhile (/= x) . sort . filter boring $ show <$> [1 .. 10000]

boring :: String -> Bool
boring x = or $ all <$> ((==) <$> ['1' .. '9']) <*> pure x
