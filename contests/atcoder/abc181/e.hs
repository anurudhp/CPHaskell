{-# LANGUAGE ParallelListComp #-}

import Control.Arrow ((>>>))
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List (sort)
import Data.Maybe (fromMaybe)

main :: IO ()
main =
  C.interact $
  C.lines >>>
  drop 1 >>> map (C.words >>> map readInt) >>> solve >>> show >>> C.pack
  where
    readInt = C.readInt >>> fromMaybe undefined >>> fst

solve :: [[Int]] -> Int
solve [hs, ws] = go (hs' ++ [inf]) ws' sums False
  where
    hs' = sort hs
    ws' = sort ws
    pre = reverse . sufAltSum . reverse $ hs'
    suf = sufAltSum hs'
    sums = zipWith (-) pre suf

sufAltSum :: [Int] -> [Int]
sufAltSum xs = scanr f 0 [(x, i) | x <- xs | i <- [1 ..]]
  where
    f (x, i) acc
      | even i = acc + x
      | odd i = acc - x

inf = 10 ^ 18

-- go hs ws sums add?
go :: [Int] -> [Int] -> [Int] -> Bool -> Int
go _ [] _ _ = inf
go (h:hs) (w:ws) sums shouldAdd
  | w <= h = min (w' + head sums) (go (h : hs) ws sums shouldAdd)
  | otherwise = go hs (w : ws) (tail sums) (not shouldAdd)
  where
    w'
      | shouldAdd = w
      | otherwise = -w
