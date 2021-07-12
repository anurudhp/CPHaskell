import Control.Arrow ((>>>))
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Maybe (fromMaybe)
import Data.Bits ((.&.), Bits (xor))

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = let (hs, ts) = splitAt k xs in hs : chunksOf k ts

main :: IO ()
main = interact $
  lines >>> drop 1 >>> chunksOf 2 >>>
    map (last >>> words >>> map read >>> solve >>> map show >>> unwords)
    >>> unlines

solve :: [Int] -> [Int]
solve = reverse . snd . foldl upd (0, [])
  where
    upd (x, ys) x' = let e = x - (x .&. x') in (x' `xor` e, e : ys)
