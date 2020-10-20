-- PROBLEM: https://cses.fi/problemset/task/1621
-- NAME: Distinct Numbers

import Control.Arrow ((>>>))
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List (group, sort)

main :: IO ()
main = C.interact $ C.words >>> drop 1 >>> map getInt >>> solve >>> show >>> C.pack
  where
    getInt s = let (Just (x, _)) = C.readInt s in x

solve :: [Int] -> Int
solve = length . group . sort
