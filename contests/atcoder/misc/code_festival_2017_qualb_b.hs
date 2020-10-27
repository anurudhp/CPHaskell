-- PROBLEM https://atcoder.jp/contests/code-festival-2017-qualb/tasks/code_festival_2017_qualb_b
{-# LANGUAGE Safe #-}

import safe Control.Arrow ((>>>))
import safe Data.Bool (bool)
import safe qualified Data.ByteString.Lazy.Char8 as C
import safe Data.List (sort)
import safe Data.Maybe (fromMaybe)

main :: IO ()
main =
  C.interact $
    C.lines >>> map (C.words >>> map readInt) >>> solve >>> bool "NO\n" "YES\n" >>> C.pack
  where
    readInt = C.readInt >>> fromMaybe undefined >>> fst

solve :: [[Int]] -> Bool
solve [_, xs, _, ys] = sort ys `subseq` sort xs
  where
    subseq [] _ = True
    subseq _ [] = False
    subseq (x : xs) (y : ys)
      | x == y = xs `subseq` ys
      | otherwise = (x : xs) `subseq` ys
