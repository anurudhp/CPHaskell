-- PROBLEM https://cses.fi/problemset/task/1083
{-# LANGUAGE ParallelListComp #-}

import Control.Arrow ((>>>))
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List (sort)
import Data.Maybe (fromMaybe)

main :: IO ()
main = C.interact $ C.words >>> map readInt >>> solve >>> show >>> C.pack
  where
    readInt = C.readInt >>> fromMaybe undefined >>> fst

solve :: [Int] -> Int
solve (n:xs) =
  fst . head . filter snd $
  [(y, x /= y) | x <- sort (n + 1 : xs) | y <- [1 .. n]]
