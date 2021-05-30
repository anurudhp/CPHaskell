import Control.Arrow
import Data.List
import Data.Bool

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (s, xs') = splitAt n xs in s : chunksOf n xs'

main = interact $ 
         lines >>> drop 1 >>> chunksOf 2 
         >>> map ((!! 1) >>> solve >>> bool "NO" "YES")
         >>> unlines

solve = all ((== 1) . length) . group . sort . map head . group
