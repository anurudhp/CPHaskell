import Control.Arrow ((>>>))
import Data.Bool (bool)
import Data.Maybe (fromMaybe)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = let (hs, ts) = splitAt k xs in hs : chunksOf k ts

main :: IO ()
main =
  interact $
    lines >>> drop 1 >>> chunksOf 4
      >>> map (drop 1 >>> map (words >>> map read) >>> solve
               >>> fmap (map show >>> unwords) >>> fromMaybe "-1")
      >>> unlines

solve :: [[Int]] -> Maybe [Int]
solve [[k,_,_], xs, ys] = build k xs ys

build :: Int -> [Int] -> [Int] -> Maybe [Int]
build _ [] [] = Just []
build k (x:xs) [] = when (x <= k) $ (x:) <$> build (updK k x) xs []
build k [] ys = build k ys []
build k (x:xs) (y:ys)
  | x <= k = (x:) <$> build (updK k x) xs (y:ys)
  | y <= k = (y:) <$> build (updK k y) (x:xs) ys
  | otherwise = Nothing

updK :: Int -> Int -> Int
updK k 0 = k + 1
updK k _ = k

when :: Bool -> Maybe a -> Maybe a
when True ma = ma
when False _ = Nothing
