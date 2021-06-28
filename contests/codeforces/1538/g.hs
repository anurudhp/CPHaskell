import Control.Arrow ((>>>))

main :: IO ()
main =
  interact $
  lines >>> drop 1 >>> map (words >>> map read >>> solve >>> show) >>> unlines

solve :: [Int] -> Int
solve [x, y, a, b]
  | a == b = min x y `div` a
  | otherwise = search 0 (1 + min x y `div` c)
  where
    c = min a b
    gap = abs (b - a)
    search l r
      | l + 1 == r = l
      | otherwise = let mid = (l + r) `div` 2
                     in if ok mid then search mid r else search l mid
    ok k =
      let m = (x - k * c) `div` gap
          n = (y - k * c) `div` gap
       in m + n >= k
