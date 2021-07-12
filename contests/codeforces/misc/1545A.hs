import Control.Arrow ((>>>))
import Data.Bool (bool)
import Data.List (group, groupBy, sort)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = let (hs, ts) = splitAt k xs in hs : chunksOf k ts

main :: IO ()
main =
  interact $
    lines >>> drop 1 >>> chunksOf 2
      >>> map (last >>> words >>> map read >>> solve >>> bool "No" "Yes")
      >>> unlines

solve :: [Int] -> Bool
solve =
  sorted
    . interleave
    . map (map snd)
    . groupBy (equating fst)
    . sort
    . zip ((`mod` 2) <$> [0 ..])

equating :: Eq a => (b -> a) -> b -> b -> Bool
equating f x y = f x == f y

sorted :: (Ord a) => [a] -> Bool
sorted xs = and $ zipWith (<=) xs (tail xs)

interleave :: [[a]] -> [a]
interleave [] = []
interleave xs = let xs' = filter (not . null) xs in map head xs' ++ interleave (map tail xs')

