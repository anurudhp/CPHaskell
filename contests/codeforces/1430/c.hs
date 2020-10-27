{-# LANGUAGE Safe #-}

import safe Control.Arrow ((>>>))

main :: IO ()
main =
  interact $
    lines
      >>> drop 1
      >>> map (read >>> solve >>> showAns)
      >>> concat
      >>> unlines
  where
    showAns (x, ys) = show x : map showII ys
    showII (x, y) = show x ++ " " ++ show y

solve :: Int -> (Int, [(Int, Int)])
solve n = (2, ((n - 1, n) :) . reverse $ zip [1 .. n] [3 .. n])
