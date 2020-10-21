{-# LANGUAGE Safe #-}

import safe Control.Arrow ((>>>))
import safe Data.Bool (bool)
import safe Data.Maybe (fromMaybe)

main :: IO ()
main =
  interact $
    lines
      >>> drop 1
      >>> map (words >>> map read)
      >>> process
      >>> map (map show >>> unwords)
      >>> unlines

process :: [[Int]] -> [[Int]]
process [] = []
process ([n] : a : rest) = solve n a ++ process rest

solve :: Int -> [Int] -> [[Int]]
solve n a = [[maximum fin], fin]
  where
    fin
      | all (== head a) a = replicate n 1
      | head cols == last cols && head a /= last a =
        fromMaybe (3 : tail cols) (update cols)
      | otherwise = cols

    cols = colour 1 a

    update :: [Int] -> Maybe [Int]
    update [] = Nothing
    update [_] = Nothing
    update (x : xs)
      | x == head xs = Just (x : map (pick True) xs)
      | otherwise = (x :) <$> update xs

    colour :: Int -> [Int] -> [Int]
    colour _ [] = []
    colour cur [_] = [cur]
    colour cur (x : x' : xs) = cur : colour (pick (x /= x') cur) (x' : xs)

    pick :: Bool -> Int -> Int
    pick fl x = bool x (3 - x) fl
