-- https://cses.fi/problemset/task/1745/
import Control.Arrow ((>>>))

main :: IO ()
main = interact $ words >>> drop 1 >>> map read >>> process >>> unlines

process :: [Int] -> [String]
process xs = [show . length, unwords . map show] <*> [solve xs]

solve :: [Int] -> [Int]
solve =
  drop 1 . map fst . filter snd . zip [0 ..] . foldl next (True : repeat False)
  where
    next dp x = zipWith (||) dp (replicate x False ++ dp)

-- Alternate solution, without infinite lists.
solve' :: [Int] -> [Int]
solve' = drop 1 . map fst . filter snd . zip [0 ..] . foldl f [True]
  where
    f dp x =
      let dp' = replicate x False
       in zipWith (||) (dp ++ dp') (dp' ++ dp)
