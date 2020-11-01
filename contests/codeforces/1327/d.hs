-- AC https://codeforces.com/contest/1327/submission/94918280
{-# LANGUAGE Safe #-}

import Control.Arrow ((>>>))
import qualified Data.ByteString.Lazy.Char8 as C
import safe Data.Graph (flattenSCC, stronglyConnComp)
import safe Data.Maybe (catMaybes, fromJust)

main :: IO ()
main =
  C.interact $
  C.lines >>>
  drop 1 >>>
  map (C.words >>> map readInt) >>>
  process >>> map (show >>> C.pack) >>> C.unlines
  where
    readInt = C.readInt >>> fromJust >>> fst

process :: [[Int]] -> [Int]
process [] = []
process ([n]:ps:cs:xs) = solve n ps cs : process xs

solve :: Int -> [Int] -> [Int] -> Int
solve _ ps cs =
  minimum $
  best . flattenSCC <$> stronglyConnComp (zip3 cs [1 ..] ((: []) <$> ps))

best :: [Int] -> Int
best xs = head $ filter check $ filter ((== 0) . (n `mod`)) [1 .. n]
  where
    n = length xs
    check :: Int -> Bool
    check len = not . null . catMaybes $ groupZip len [] xs
    groupZip :: Int -> [Maybe Int] -> [Int] -> [Maybe Int]
    groupZip _ cur [] = cur
    groupZip len [] xs = groupZip len (Just <$> take len xs) (drop len xs)
    groupZip len cur xs =
      groupZip len (zipWith comb cur (take len xs)) (drop len xs)
      where
        comb Nothing _ = Nothing
        comb (Just x) y =
          if x == y
            then Just x
            else Nothing
