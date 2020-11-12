{-# LANGUAGE ParallelListComp #-}

-- https://cses.fi/problemset/task/1745/
import Control.Arrow ((>>>))

main :: IO ()
main = interact $ words >>> drop 1 >>> map read >>> process >>> unlines

process :: [Int] -> [String]
process xs = [show . length, unwords . map show] <*> [solve xs]

solve :: [Int] -> [Int]
solve xs = dpIxsTrue
  where
    update dp x = zipWith (||) dp (replicate x False ++ dp)
    dp = foldl update (True : repeat False) xs
    dpWithIxs = [(i, p) | i <- [0 .. sum xs] | p <- dp]
    dpIxsTrue = [i | (i, p) <- dpWithIxs, p == True, i > 0]

-- Alternate solution, without infinite lists.
solve' :: [Int] -> [Int]
solve' = drop 1 . map fst . filter snd . zip [0 ..] . foldl f [True]
  where
    f ps x = zipWith (||) (ps ++ ps') (ps' ++ ps)
      where
        ps' = replicate x False
