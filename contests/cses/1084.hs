-- PROBLEM: https://cses.fi/problemset/task/1084/
-- NAME: Apartments

import Control.Arrow ((>>>))
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Foldable (Foldable (toList))
import Data.Sequence (fromList, sort)

main :: IO ()
main = C.interact $ C.words >>> map getInt >>> solve >>> show >>> (++ "\n") >>> C.pack
  where
    getInt s = let (Just (x, _)) = C.readInt s in x

solve :: [Int] -> Int
solve (n : _ : k : zs) = go as bs
  where
    as = toList . sort . fromList $ take n zs
    bs = toList . sort . fromList $ drop n zs

    go [] _ = 0
    go _ [] = 0
    go (x : xs) (y : ys)
      | abs (x - y) <= k = 1 + go xs ys
      | x < y = go xs (y : ys)
      | otherwise = go (x : xs) ys
