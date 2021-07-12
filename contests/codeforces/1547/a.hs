import Control.Arrow ((>>>))
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Maybe (fromMaybe)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = let (hs, ts) = splitAt k xs in hs : chunksOf k ts

main :: IO ()
main = interact $ lines >>> drop 1 >>> chunksOf 4 >>> map (drop 1 >>> map (words >>> map read) >>> solve >>> show) >>> unlines

solve :: [[Int]] -> Int
solve [[xa, ya], [xb, yb], [xf, yf]]
  | xa == xf && xf == xb && min ya yb <= yf && yf <= max ya yb = gap + 2
  | ya == yf && yf == yb && min xa xb <= xf && xf <= max xa xb = gap + 2
  | otherwise = gap
  where
    gap = abs (xa - xb) + abs (ya - yb)
