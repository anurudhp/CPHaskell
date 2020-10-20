-- PROBLEM: https://cses.fi/problemset/task/1646
-- NAME: Range Sum Queries I

import Control.Arrow ((>>>))
import qualified Data.ByteString.Lazy.Char8 as C

main :: IO ()
main = C.interact $ C.words >>> map getInt >>> solve >>> map (show >>> C.pack) >>> C.unlines
  where
    getInt s = let (Just (x, _)) = C.readInt s in x

solve :: [Int] -> [Int]
solve (n : _ : xs) = go (drop n xs)
  where
    st = build 0 n (take n xs)
    f l r = query (l - 1) r st
    go [] = []
    go (l : r : qs) = f l r : go qs

data ST = STLeaf | STNode Int Int Int ST ST

value :: ST -> Int
value STLeaf = 0
value (STNode _ _ v _ _) = v

build :: Int -> Int -> [Int] -> ST
build _ _ [] = STLeaf
build l r [x] = STNode l r x STLeaf STLeaf
build l r xs = STNode l r v lt rt
  where
    m = (l + r) `div` 2
    lt = build l m (take (m - l) xs)
    rt = build m r (drop (m - l) xs)
    v = value lt + value rt

query :: Int -> Int -> ST -> Int
query _ _ STLeaf = 0
query l r (STNode l' r' v lt rt)
  | l >= r' || r <= l' = 0
  | l <= l' && r' <= r = v
  | otherwise = query l r lt + query l r rt
