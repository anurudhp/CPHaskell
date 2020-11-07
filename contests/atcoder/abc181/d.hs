import Control.Arrow ((>>>))
import Data.Bool (bool)
import Data.List (group, sort)

main :: IO ()
main = interact $ words >>> head >>> solve >>> bool "No" "Yes"

solve :: String -> Bool
solve s
  | length s == 1 = s == "8"
  | length s == 2 = read s `mod` 8 == 0 || read (reverse s) `mod` 8 == 0
  | otherwise = any (subset hs) mhs
  where
    histogram = map (\gs -> (head gs, length gs)) . group . sort
    multiples = map show . filter ((== 0) . (`mod` 8)) $ [100 .. 999]
    hs = histogram s
    mhs = map histogram multiples

type Histogram a = [(a, Int)]

-- subset takes a key-sorted histogram
-- subset xs ys = ys \subset xs
subset :: Eq a => Histogram a -> Histogram a -> Bool
subset _ [] = True
subset [] _ = False
subset (x:xs) (y:ys)
  | fst x == fst y = snd x >= snd y && subset xs ys
  | otherwise = subset xs (y : ys)
