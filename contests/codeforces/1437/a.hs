{-# LANGUAGE Safe #-}

import Control.Arrow ((>>>))
import Data.Bool (bool)

main =
  interact $
  lines >>>
  drop 1 >>> map (words >>> map read >>> solve >>> bool "NO" "YES") >>> unlines

solve :: [Int] -> Bool
solve [l, r] = r < 2 * l
