-- https://cses.fi/problemset/task/1745/

import Control.Arrow ((>>>))

main :: IO ()
main = interact $ words >>> drop 1 >>> map read >>> process >>> unlines

process :: [Int] -> [String]
process xs = [show . length, unwords . map show] <*> [solve xs]

solve :: [Int] -> [Int]
solve = drop 1 . map fst . filter snd . zip [0 ..] . foldl f [True]
  where
    f ps x = zipWith (||) (ps ++ ps') (ps' ++ ps)
      where
        ps' = replicate x False
