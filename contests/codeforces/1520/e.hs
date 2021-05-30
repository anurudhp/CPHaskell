import Control.Arrow
import Data.List(sort, group)

chunksOf :: Int -> [a] -> [[a]]
chunksOf k [] = []
chunksOf k xs = let (g, xs') = splitAt k xs in g : chunksOf k xs'

type LL = Integer

main = interact $ 
         lines >>> drop 1 >>> chunksOf 2
         >>> map ((!! 1) >>> solve >>> show)
         >>> unlines

solve :: String -> LL
solve s = 
    let xs = zipWith (-) [i | (c, i) <- zip s [0..], c == '*'] [0..]
        m = xs !! (length xs `div` 2)
    in sum $ map (abs . (m -)) xs
    
