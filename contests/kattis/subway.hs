-- https://open.kattis.com/problems/subway
{-# LANGUAGE Safe #-}

import safe Control.Arrow ((>>>))
import safe Data.Bool (bool)
import safe Data.List (sort)
import Data.List.Split (chunksOf)
import safe Text.Parsec (char, many, parse)

main :: IO ()
main =
  interact $
    lines
      >>> drop 1
      >>> chunksOf 2
      >>> map (solve >>> bool "different" "same")
      >>> unlines

solve :: [String] -> Bool
solve [t1, t2] = buildTree t1 == buildTree t2

newtype Tree = Node [Tree] deriving (Show, Eq, Ord)

buildTree :: String -> Tree
buildTree = either undefined id . parse parseTree ""
  where
    parseTree = Node . sort <$> many (char '0' *> parseTree <* char '1')
