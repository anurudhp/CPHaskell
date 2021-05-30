import Control.Arrow
import Data.List(sort, group)

chunksOf :: Int -> [a] -> [[a]]
chunksOf k [] = []
chunksOf k xs = let (g, xs') = splitAt k xs in g : chunksOf k xs'

type LL = Integer

main = interact $ 
         lines >>> drop 1 >>> chunksOf 2
         >>> map ((!! 1) >>> words >>> map read >>> solve >>> show)
         >>> unlines

solve :: [Int] -> LL
solve = sum . map (choose2 . length) . group . sort . zipWith (-) [0..]

choose2 :: Int -> LL
choose2 n = let n' = toInteger n in (n' * (n' - 1)) `div` 2
