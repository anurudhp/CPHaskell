-- https://open.kattis.com/problems/ceiling
{-# LANGUAGE Safe #-}

import safe Control.Arrow ((>>>))
import safe Data.List (nub)

main :: IO ()
main = interact $ lines >>> drop 1 >>> map (words >>> map read) >>> solve >>> show

data Tree = TNode Int Tree Tree | TNull

instance (Eq Tree) where
  (==) TNull TNull = True
  (==) (TNode _ l r) (TNode _ l' r') = l == l' && r == r'
  (==) _ _ = False

solve :: [[Int]] -> Int
solve = length . nub . map (foldl insert TNull)
  where
    insert TNull x = TNode x TNull TNull
    insert (TNode y l r) x
      | x < y = TNode y (insert l x) r
      | x > y = TNode y l (insert r x)
