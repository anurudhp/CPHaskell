-- AC https://codeforces.com/contest/1399/submission/94927707
{-# LANGUAGE Safe #-}

import safe Control.Arrow ((>>>))
import safe Data.List (group, sort)

main :: IO ()
main =
  interact $
    lines >>> drop 1
      >>> map (words >>> map read)
      >>> process
      >>> map show
      >>> unlines

process :: [[Int]] -> [Int]
process [] = []
process ([_] : xs : rest) = solve xs : process rest

solve :: [Int] -> Int
solve [_] = 0
solve xs =
  maximum $
    (map length . group . sort . map (uncurry (+)) . filter (uncurry (<=)) . concatMap dropEqDup)
      (zip <$> gs <*> gs)
  where
    gs = group . sort $ xs

    dropEqDup :: [(Int, Int)] -> [(Int, Int)]
    dropEqDup xs
      | uncurry (==) (head xs) = take (length xs `div` 2) xs
      | otherwise = xs
